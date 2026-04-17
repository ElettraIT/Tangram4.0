       Identification Division.
       Program-Id.                                 pdcf4020           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcf                 *
      *                                Settore:    com                 *
      *                                   Fase:    dcf402              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 22/02/95    *
      *                       Ultima revisione:    NdK del 05/11/18    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Richieste per il programma pdcf4021:        *
      *                                                                *
      *                    Stampa anagrafica commerciale fornitori     *
      *                                                                *
      *                    - pdcf4021 : Elenco anagrafiche con o senza *
      *                                 voci aggiuntive                *
      *                    - pdcf4022 : Scheda                         *
      *                    - pdcf4023 : Etichette o archivio sequen-   *
      *                                 ziale (*)                      *
      *                                                                *
      *                (*) N.B.: L'archivio sequenziale viene creato   *
      *                          nel pathname (/abd/asc/[utente]/dcf)  *
      *                          con nome 'FORNITOR.TXT' ed e' separa- *
      *                          to da TAB, se richiesto anagrafico    *
      *                          completo o 'FOR_MAIL.TXT' se lista di *
      *                          indirizzi e-mail                      *
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
                     "dcf"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "com"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "dcf402"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pdcf4020"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "STAMPA ANAGRAFICA COMMERCIALE FORNITORI "       .

      *    *===========================================================*
      *    * Area per il programma di esecuzione                       *
      *    *-----------------------------------------------------------*
       01  i-exe.
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma di esecuzione             *
      *        *-------------------------------------------------------*
           05  i-exe-pro                  pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Pathname del programma di esecuzione                  *
      *        *-------------------------------------------------------*
           05  i-exe-pat                  pic  x(40)                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "msegrt" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mpslct" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/r"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli                 "mbckgx" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/b"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mmessg" *
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
      *            * Per routine sel-prm-stp-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-sel-prm-stp      pic  x(01)                  .
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
      *            * Si/No richieste per programma di esecuzione       *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-ric      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No richiesta di selezione stampa               *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-stp      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area per preparazione parametri selezione stampa      *
      *        *-------------------------------------------------------*
           05  w-cnt-stp.
               10  w-cnt-stp-tip-sel      pic  x(10)                  .
               10  w-cnt-stp-cod-stp      pic  x(08)                  .
               10  w-cnt-stp-tip-sta      pic  x(01)                  .
               10  w-cnt-stp-cod-mod      pic  x(08)                  .
               10  w-cnt-stp-tip-mod      pic  x(01)                  .
               10  w-cnt-stp-amp-lin      pic  9(03)                  .
               10  w-cnt-stp-top-lin      pic  9(04)                  .
               10  w-cnt-stp-lin-min      pic  9(02)                  .
               10  w-cnt-stp-bot-lin      pic  9(04)                  .
               10  w-cnt-stp-amp-car      pic  9(02)v9(02)            .
               10  w-cnt-stp-alt-int      pic  9(02)v9(02)            .
               10  w-cnt-stp-esp-fut      pic  x(99)                  .
               10  w-cnt-stp-fnz-spc      pic  x(99)                  .
      *        *-------------------------------------------------------*
      *        * Work per string-unstring record richieste             *
      *        *-------------------------------------------------------*
           05  w-stu-rrr.
               10  w-stu-rrr-pnt-stu      pic  9(05)                  .
               10  w-stu-rrr-255-byt.
                   15  filler occurs 255  pic  x(01)                  .
               10  w-stu-rrr-sav-pnt      pic  9(05)                  .

      *    *===========================================================*
      *    * Records files                                             *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [cbp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfcbp"                          .
      *        *-------------------------------------------------------*
      *        * [yst]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfyst"                          .

      *    *===========================================================*
      *    * Work-area richieste per stampa                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/pdcf4020.pgl"                   .

      *    *===========================================================*
      *    * Work-area per accettazioni/visualizzazioni                *
      *    *-----------------------------------------------------------*
       01  w-acc.
      *        *-------------------------------------------------------*
      *        * Codici di avviamento postale                          *
      *        *-------------------------------------------------------*
           05  w-acc-cod-cap.
      *            *---------------------------------------------------*
      *            * Indici                                            *
      *            *---------------------------------------------------*
               10  w-acc-cod-cap-max      pic  9(02)         value 02 .
               10  w-acc-cod-cap-inx      pic  9(02)                  .
               10  w-acc-cod-cap-i02      pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Tipo ordinamento stampa                               *
      *        *-------------------------------------------------------*
           05  w-sav-tip-ors              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo ordinamento anagrafica fornitori                 *
      *        *-------------------------------------------------------*
           05  w-sav-ana-fnt              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo ordinamento anagrafica zone                      *
      *        *-------------------------------------------------------*
           05  w-sav-ana-zon              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo ordinamento anagrafica categorie                 *
      *        *-------------------------------------------------------*
           05  w-sav-ana-cat              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo ordinamento anagrafica codici statistici         *
      *        *-------------------------------------------------------*
           05  w-sav-ana-stt              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo ordinamento anagrafica forme di pagamento        *
      *        *-------------------------------------------------------*
           05  w-sav-ana-fop              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo ordinamento anagrafica nazioni                   *
      *        *-------------------------------------------------------*
           05  w-sav-ana-naz              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo ordinamento anagrafica regioni                   *
      *        *-------------------------------------------------------*
           05  w-sav-ana-rgn              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo ordinamento anagrafica provincie                 *
      *        *-------------------------------------------------------*
           05  w-sav-ana-prv              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo ordinamento anagrafica comuni                    *
      *        *-------------------------------------------------------*
           05  w-sav-ana-cmn              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo stampa                                           *
      *        *-------------------------------------------------------*
           05  w-sav-tip-stp              pic  9(02)                  .

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
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [yst]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-yst.
               10  w-let-arc-yst-flg      pic  x(01)                  .
               10  w-let-arc-yst-tip      pic  9(02)                  .
               10  w-let-arc-yst-cod      pic  9(05)                  .
               10  w-let-arc-yst-des      pic  x(20)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice fornitore commerciale   *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmndcf0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice nostra cassa, banca, o  *
      *    * c/c postale                                               *
      *    *-----------------------------------------------------------*
           copy      "pgm/gep/prg/cpy/acdecbp0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice zona fornitori          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmnyst1.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice categoria fornitori     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmnyst2.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice statistico fornitori    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmnyst3.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione forma di pagamento             *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmnyfp0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice nazione                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/acdenaz0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice regione                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/acdergn0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice provincia               *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/acdeprv0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione comune                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/acomgeo0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione frazione                       *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/afrageo0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione localita'                      *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/alocgeo0.acl"                   .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo ordinamento stampa                    *
      *        *-------------------------------------------------------*
           05  w-exp-tip-ors.
               10  w-exp-tip-ors-num      pic  9(02)       value 11   .
               10  w-exp-tip-ors-lun      pic  9(02)       value 40   .
               10  w-exp-tip-ors-tbl.
                   15  filler             pic  x(40) value
                            "Per fornitore                           ".
                   15  filler             pic  x(40) value
                            "Per zona                                ".
                   15  filler             pic  x(40) value
                            "Per categoria                           ".
                   15  filler             pic  x(40) value
                            "Per codice statistico                   ".
                   15  filler             pic  x(40) value
                            "Per forma di pagamento                  ".
                   15  filler             pic  x(40) value
                            "Per nazione                             ".
                   15  filler             pic  x(40) value
                            "Per regione                             ".
                   15  filler             pic  x(40) value
                            "Per provincia                           ".
                   15  filler             pic  x(40) value
                            "Per comune                              ".
                   15  filler             pic  x(40) value
                            "Per C.a.p.                              ".
                   15  filler             pic  x(40) value
                            "Per C.a.p. e stradario                  ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo ordinamento fornitori                 *
      *        *-------------------------------------------------------*
           05  w-exp-ana-fnt.
               10  w-exp-ana-fnt-num      pic  9(02)       value 3    .
               10  w-exp-ana-fnt-lun      pic  9(02)       value 40   .
               10  w-exp-ana-fnt-tbl.
                   15  filler             pic  x(40) value
                            "Per ragione sociale                     ".
                   15  filler             pic  x(40) value
                            "Per codice fornitore                    ".
                   15  filler             pic  x(40) value
                            "Per mnemonico                           ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo ordinamento zone                      *
      *        *-------------------------------------------------------*
           05  w-exp-ana-zon.
               10  w-exp-ana-zon-num      pic  9(02)       value 3    .
               10  w-exp-ana-zon-lun      pic  9(02)       value 40   .
               10  w-exp-ana-zon-tbl.
                   15  filler             pic  x(40) value
                            "Per codice                              ".
                   15  filler             pic  x(40) value
                            "Per descrizione                         ".
                   15  filler             pic  x(40) value
                            "Per mnemonico                           ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo ordinamento categorie                 *
      *        *-------------------------------------------------------*
           05  w-exp-ana-cat.
               10  w-exp-ana-cat-num      pic  9(02)       value 3    .
               10  w-exp-ana-cat-lun      pic  9(02)       value 40   .
               10  w-exp-ana-cat-tbl.
                   15  filler             pic  x(40) value
                            "Per codice                              ".
                   15  filler             pic  x(40) value
                            "Per descrizione                         ".
                   15  filler             pic  x(40) value
                            "Per mnemonico                           ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo ordinamento codici statistici         *
      *        *-------------------------------------------------------*
           05  w-exp-ana-stt.
               10  w-exp-ana-stt-num      pic  9(02)       value 3    .
               10  w-exp-ana-stt-lun      pic  9(02)       value 40   .
               10  w-exp-ana-stt-tbl.
                   15  filler             pic  x(40) value
                            "Per codice                              ".
                   15  filler             pic  x(40) value
                            "Per descrizione                         ".
                   15  filler             pic  x(40) value
                            "Per mnemonico                           ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo ordinamento forme di pagamento        *
      *        *-------------------------------------------------------*
           05  w-exp-ana-fop.
               10  w-exp-ana-fop-num      pic  9(02)       value 3    .
               10  w-exp-ana-fop-lun      pic  9(02)       value 40   .
               10  w-exp-ana-fop-tbl.
                   15  filler             pic  x(40) value
                            "per Codice                              ".
                   15  filler             pic  x(40) value
                            "per Descrizione                         ".
                   15  filler             pic  x(40) value
                            "per Mnemonico                           ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo ordinamento nazioni                   *
      *        *-------------------------------------------------------*
           05  w-exp-ana-naz.
               10  w-exp-ana-naz-num      pic  9(02)       value 2    .
               10  w-exp-ana-naz-lun      pic  9(02)       value 40   .
               10  w-exp-ana-naz-tbl.
                   15  filler             pic  x(40) value
                            "Per codice                              ".
                   15  filler             pic  x(40) value
                            "Per descrizione                         ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo ordinamento regioni                   *
      *        *-------------------------------------------------------*
           05  w-exp-ana-rgn.
               10  w-exp-ana-rgn-num      pic  9(02)       value 2    .
               10  w-exp-ana-rgn-lun      pic  9(02)       value 40   .
               10  w-exp-ana-rgn-tbl.
                   15  filler             pic  x(40) value
                            "Per codice                              ".
                   15  filler             pic  x(40) value
                            "Per descrizione                         ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo ordinamento provincie                 *
      *        *-------------------------------------------------------*
           05  w-exp-ana-prv.
               10  w-exp-ana-prv-num      pic  9(02)       value 2    .
               10  w-exp-ana-prv-lun      pic  9(02)       value 40   .
               10  w-exp-ana-prv-tbl.
                   15  filler             pic  x(40) value
                            "Per codice                              ".
                   15  filler             pic  x(40) value
                            "Per descrizione                         ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo ordinamento comuni                    *
      *        *-------------------------------------------------------*
           05  w-exp-ana-cmn.
               10  w-exp-ana-cmn-num      pic  9(02)       value 2    .
               10  w-exp-ana-cmn-lun      pic  9(02)       value 40   .
               10  w-exp-ana-cmn-tbl.
                   15  filler             pic  x(40) value
                            "Per descrizione                         ".
                   15  filler             pic  x(40) value
                            "Per C.a.p. del comune                   ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo ordinamento fornitori a parita' di    *
      *        *            ordinamento                                *
      *        *-------------------------------------------------------*
           05  w-exp-tip-ocp.
               10  w-exp-tip-ocp-num      pic  9(02)       value 3    .
               10  w-exp-tip-ocp-lun      pic  9(02)       value 40   .
               10  w-exp-tip-ocp-tbl.
                   15  filler             pic  x(40) value
                            "Per ragione sociale                     ".
                   15  filler             pic  x(40) value
                            "Per codice fornitore                    ".
                   15  filler             pic  x(40) value
                            "Per mnemonico                           ".
      *        *-------------------------------------------------------*
      *        * Work per : Si/no salto pagina a rottura               *
      *        *-------------------------------------------------------*
           05  w-exp-sns-prt.
               10  w-exp-sns-prt-num      pic  9(02)       value 2    .
               10  w-exp-sns-prt-lun      pic  9(02)       value 02   .
               10  w-exp-sns-prt-tbl.
                   15  filler             pic  x(02)       value "Si" .
                   15  filler             pic  x(02)       value "No" .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo stampa                                *
      *        *-------------------------------------------------------*
           05  w-exp-tip-stp.
               10  w-exp-tip-stp-num      pic  9(02)       value 6    .
               10  w-exp-tip-stp-lun      pic  9(02)       value 40   .
               10  w-exp-tip-stp-tbl.
                   15  filler             pic  x(40) value
                            "Semplice elenco delle anagrafiche       ".
                   15  filler             pic  x(40) value
                            "Semplice elenco ma con voci aggiuntive  ".
                   15  filler             pic  x(40) value
                            "Scheda completa per ogni fornitore      ".
                   15  filler             pic  x(40) value
                            "Etichette per corrispondenza            ".
                   15  filler             pic  x(40) value
                            "Etichette per uso interno               ".
                   15  filler             pic  x(40) value
                            "Archivio sequenziale per esportazione   ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo etichette                             *
      *        *-------------------------------------------------------*
           05  w-exp-tip-eti.
               10  w-exp-tip-eti-num      pic  9(02)       value 2    .
               10  w-exp-tip-eti-lun      pic  9(02)       value 40   .
               10  w-exp-tip-eti-tbl.
                   15  filler             pic  x(40) value
                         "Modulo cont. 12''   - 1 c. x 8 etichette"   .
                   15  filler             pic  x(40) value
                         "Laser A4            - 3 c. x 8 etichette"   .
      *        *-------------------------------------------------------*
      *        * Work per : Voci aggiuntive da stampare                *
      *        *                                                       *
      *        * N.B.: max 18 voci !!!                                 *
      *        *-------------------------------------------------------*
           05  w-exp-vag-dst.
               10  w-exp-vag-dst-num      pic  9(02)       value 16   .
               10  w-exp-vag-dst-lun      pic  9(02)       value 40   .
               10  w-exp-vag-dst-tbl.
                   15  filler             pic  x(40) value
                            "[ ] Zona                                ".
                   15  filler             pic  x(40) value
                            "[ ] Categoria                           ".
                   15  filler             pic  x(40) value
                            "[ ] Classificatore statistico           ".
                   15  filler             pic  x(40) value
                            "[ ] Condizioni di pagamento             ".
                   15  filler             pic  x(40) value
                            "[ ] Banca d'appoggio del fornitore      ".
                   15  filler             pic  x(40) value
                            "[ ] Nostra banca d'appoggio             ".
                   15  filler             pic  x(40) value
                            "[ ] Numero di telefono                  ".
                   15  filler             pic  x(40) value
                            "[ ] Numero di Fax                       ".
                   15  filler             pic  x(40) value
                            "[ ] Numero di cellulare                 ".
                   15  filler             pic  x(40) value
                            "[ ] Indirizzo e-mail                    ".
                   15  filler             pic  x(40) value
                            "[ ] Indirizzo PEC                       ".
                   15  filler             pic  x(40) value
                            "[ ] Codice destinatario SDI             ".
                   15  filler             pic  x(40) value
                            "[ ] Nominativo interlocutore            ".
                   15  filler             pic  x(40) value
                            "[ ] Partita I.v.a.                      ".
                   15  filler             pic  x(40) value
                            "[ ] Codice Fiscale                      ".
                   15  filler             pic  x(40) value
                            "[ ] Voci descrittive                    ".
      *        *-------------------------------------------------------*
      *        * Work per : Selezione su status commerciale            *
      *        *-------------------------------------------------------*
           05  w-exp-sta-tus.
               10  w-exp-sta-tus-num      pic  9(02)       value 9    .
               10  w-exp-sta-tus-lun      pic  9(02)       value 40   .
               10  w-exp-sta-tus-tbl.
                   15  filler             pic  x(40) value
                            "[ ] Normale                             ".
                   15  filler             pic  x(40) value
                            "[ ] Esauriti i rapporti commerciali     ".
                   15  filler             pic  x(40) value
                            "[ ] Sostituito da ns. nuovo fornitore   ".
                   15  filler             pic  x(40) value
                            "[ ] Cessata attivita'                   ".
                   15  filler             pic  x(40) value
                            "[ ] Cessata attivita' ma sostituito     ".
                   15  filler             pic  x(40) value
                            "[ ] In contenzioso                      ".
                   15  filler             pic  x(40) value
                            "[ ] In contenzioso ma sostituito        ".
                   15  filler             pic  x(40) value
                            "[ ] Fallito                             ".
                   15  filler             pic  x(40) value
                            "[ ] Fallito ma sostituito               ".

      *    *===========================================================*
      *    * Work-area per valori di i.p.c.                            *
      *    *-----------------------------------------------------------*
       01  w-ipc.
      *        *-------------------------------------------------------*
      *        * Variabili di i.p.c. da livello precedente             *
      *        *-------------------------------------------------------*
           05  w-ipc-dlp.
      *            *---------------------------------------------------*
      *            * Per stampa precablata                             *
      *            *---------------------------------------------------*
               10  w-ipc-dlp-stp.
      *                *-----------------------------------------------*
      *                * Codice fornitore                              *
      *                *-----------------------------------------------*
                   15  w-ipc-dlp-stp-fnt  pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Dipendenza                                    *
      *                *-----------------------------------------------*
                   15  w-ipc-dlp-stp-dpz  pic  x(04)                  .

      *    *===========================================================*
      *    * Work-area per coordinate di posizionamento campi          *
      *    *-----------------------------------------------------------*
       01  w-lpt.
      *        *-------------------------------------------------------*
      *        * Linea e posizione per Tipo ordinamento stampa         *
      *        *-------------------------------------------------------*
           05  w-lpt-tip-ors-lin          pic  9(03)    value 004     .
           05  w-lpt-tip-ors-pos          pic  9(03)    value 040     .
           05  w-lpt-tip-ors-ppm          pic  9(03)    value 001     .
      *        *-------------------------------------------------------*
      *        * Linea e posizione per Tipo ordinamento anagrafica     *
      *        *-------------------------------------------------------*
           05  w-lpt-ord-ana-lin          pic  9(03)    value 005     .
           05  w-lpt-ord-ana-pos          pic  9(03)    value 040     .
           05  w-lpt-ord-ana-ppm          pic  9(03)    value 001     .
      *        *-------------------------------------------------------*
      *        * Linea e posizione per Selettore iniziale anagrafiche  *
      *        *-------------------------------------------------------*
           05  w-lpt-sel-ain-lin          pic  9(03)    value 007     .
           05  w-lpt-sel-ain-pos          pic  9(03)    value 040     .
           05  w-lpt-sel-ain-ppm          pic  9(03)    value 001     .
      *        *-------------------------------------------------------*
      *        * Linea e posizione per Selettore finale anagrafiche    *
      *        *-------------------------------------------------------*
           05  w-lpt-sel-afi-lin          pic  9(03)    value 008     .
           05  w-lpt-sel-afi-pos          pic  9(03)    value 040     .
           05  w-lpt-sel-afi-ppm          pic  9(03)    value 001     .
      *        *-------------------------------------------------------*
      *        * Linea e posizione per Tipo ordinamento fornitori a    *
      *        * parita' di                                            *
      *        *-------------------------------------------------------*
           05  w-lpt-tip-ocp-lin          pic  9(03)    value 010     .
           05  w-lpt-tip-ocp-pos          pic  9(03)    value 040     .
           05  w-lpt-tip-ocp-ppm          pic  9(03)    value 001     .
      *        *-------------------------------------------------------*
      *        * Linea e posizione per Voci aggiuntive da stampare     *
      *        *-------------------------------------------------------*
           05  w-lpt-vag-dst-lin          pic  9(03)    value 013     .
           05  w-lpt-vag-dst-pos          pic  9(03)    value 040     .
           05  w-lpt-vag-dst-ppm          pic  9(03)    value 001     .
      *        *-------------------------------------------------------*
      *        * Linea e posizione per Si/no salto pagina a rottura    *
      *        *-------------------------------------------------------*
           05  w-lpt-sns-prt-lin          pic  9(03)    value 014     .
           05  w-lpt-sns-prt-pos          pic  9(03)    value 040     .
           05  w-lpt-sns-prt-ppm          pic  9(03)    value 001     .

      *    *===========================================================*
      *    * Work-area per esecuzione programma di overlay             *
      *    *-----------------------------------------------------------*
       01  w-ovy.
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma di esecuzione             *
      *        *-------------------------------------------------------*
           05  w-ovy-exe-pro              pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Pathname del programma di esecuzione                  *
      *        *-------------------------------------------------------*
           05  w-ovy-exe-pat              pic  x(40)                  .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err con box centrale, o per messaggi centra- *
      *        * li circondati da un box                               *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(65)                  .
               10  w-err-box-err-m02      pic  x(65)                  .

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
      *              * Preparazione tipo funzionamento programma       *
      *              *-------------------------------------------------*
           perform   pre-tip-fun-000      thru pre-tip-fun-999        .
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           perform   rou-opn-fls-000      thru rou-opn-fls-999        .
           if        w-cnt-rou-opn-fls    not  = spaces
                     go to main-750.
      *              *-------------------------------------------------*
      *              * Se no richieste : a selezione stampante         *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S"
                     go to main-350.
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
       main-350.
      *              *-------------------------------------------------*
      *              * Se no stampa : ad esecuzione                    *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-stp    not  = "S"
                     go to main-450.
      *              *-------------------------------------------------*
      *              * Preparazione defaults per parametri di selezio- *
      *              * ne stampa                                       *
      *              *-------------------------------------------------*
           perform   pre-prm-stp-000      thru pre-prm-stp-999        .
      *              *-------------------------------------------------*
      *              * Selezione parametri stampa                      *
      *              *-------------------------------------------------*
           perform   sel-prm-stp-000      thru sel-prm-stp-999        .
      *                  *---------------------------------------------*
      *                  * Test se uscita                              *
      *                  *---------------------------------------------*
           if        w-cnt-sel-prm-stp    not  = spaces
                     go to main-750.
       main-450.
      *                  *---------------------------------------------*
      *                  * Richiesta alla segreteria se funzionamento  *
      *                  * in background o foreground                  *
      *                  *---------------------------------------------*
           move      "BF"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Se esecuzione in foreground                 *
      *                  *---------------------------------------------*
           if        s-snb                =    "B"
                     go to main-500.
           perform   exe-pgm-frg-000      thru exe-pgm-frg-999        .
           go to     main-750.
       main-500.
      *                  *---------------------------------------------*
      *                  * Se esecuzione in background                 *
      *                  *---------------------------------------------*
           perform   exe-pgm-bkg-000      thru exe-pgm-bkg-999        .
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
      *    * Dichiarazione di inizio programma                         *
      *    *-----------------------------------------------------------*
       dic-ini-pgm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dic-ini-pgm      .
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
      *    * Programma di esecuzione in foreground                     *
      *    *-----------------------------------------------------------*
       exe-pgm-frg-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione rullino messaggi di foreground *
      *              *-------------------------------------------------*
           move      "OF"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Se errore : uscita                          *
      *                  *---------------------------------------------*
            if       m-rsc                not  = spaces
                     go to exe-pgm-frg-999.
      *              *-------------------------------------------------*
      *              * Scrittura record richieste per foreground       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio scrittura record richieste           *
      *                  *---------------------------------------------*
           move      "OO"                 to   b-ope                  .
           move      "F"                  to   b-tfe                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
      *                      *-----------------------------------------*
      *                      * Se errore : uscita                      *
      *                      *-----------------------------------------*
            if       b-rsc                not  = spaces
                     go to exe-pgm-frg-999.
      *                  *---------------------------------------------*
      *                  * Estrazione segmenti da 255  bytes da record *
      *                  * richieste                                   *
      *                  *---------------------------------------------*
           move      1                    to   w-stu-rrr-pnt-stu      .
       exe-pgm-frg-200.
           move      spaces               to   w-stu-rrr-255-byt      .
           move      w-stu-rrr-pnt-stu    to   w-stu-rrr-sav-pnt      .
           unstring  rr                   into w-stu-rrr-255-byt
                                  with pointer w-stu-rrr-pnt-stu      .
           move      w-stu-rrr-255-byt    to   b-chr                  .
           if        w-stu-rrr-pnt-stu    =    w-stu-rrr-sav-pnt
                     go to exe-pgm-frg-400.
           move      "PT"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
           go to     exe-pgm-frg-200.
       exe-pgm-frg-400.
      *                  *---------------------------------------------*
      *                  * Fine scrittura record richieste             *
      *                  *---------------------------------------------*
           move      "CL"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
      *                      *-----------------------------------------*
      *                      * Se errore : uscita                      *
      *                      *-----------------------------------------*
            if       b-rsc                not  = spaces
                     go to exe-pgm-frg-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Lancio del programma di esecuzione              *
      *              *-------------------------------------------------*
           call      i-exe-pat                                        .
      *              *-------------------------------------------------*
      *              * Cancel del programma di esecuzione              *
      *              *-------------------------------------------------*
           cancel    i-exe-pat                                        .
      *              *-------------------------------------------------*
      *              * Cancel del modulo "mbckgr"                      *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mbckgr"                         .
      *              *-------------------------------------------------*
      *              * Visualizzazione eventuali errori di esecuzione  *
      *              *-------------------------------------------------*
           move      "VE"                 to   b-ope                  .
           move      "F"                  to   b-tfe                  .
           move      i-ide-des            to   b-chr                  .
           call      "swd/mod/prg/obj/mbckgv"
                                         using b                      .
           cancel    "swd/mod/prg/obj/mbckgv"                         .
       exe-pgm-frg-999.
           exit.

      *    *===========================================================*
      *    *  Selezione parametri stampa                               *
      *    *-----------------------------------------------------------*
       sel-prm-stp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sel-prm-stp      .
      *              *-------------------------------------------------*
      *              * Test se selezione stampa da eseguire            *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-stp    not  = "S"
                     go to sel-prm-stp-999.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per richiamo selezione   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Informazioni generali da segreteria         *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Codice azienda                          *
      *                      *-----------------------------------------*
           move      s-azi                to   r-env-cod-azi          .
      *                      *-----------------------------------------*
      *                      * Codice terminale                        *
      *                      *-----------------------------------------*
           move      s-ter                to   r-env-cod-ter          .
      *                      *-----------------------------------------*
      *                      * Codice utente                           *
      *                      *-----------------------------------------*
           move      s-ute                to   r-env-cod-ute          .
      *                      *-----------------------------------------*
      *                      * Date and time da segreteria             *
      *                      *-----------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Date and time                           *
      *                      *-----------------------------------------*
           move      s-sdt                to   r-env-dat-tim          .
      *                  *---------------------------------------------*
      *                  * Informazioni da identificazione programma   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Sistema applicativo                     *
      *                      *-----------------------------------------*
           move      i-ide-sap            to   r-ide-sis-app          .
      *                      *-----------------------------------------*
      *                      * Area gestionale                         *
      *                      *-----------------------------------------*
           move      i-ide-arg            to   r-ide-are-ges          .
      *                      *-----------------------------------------*
      *                      * Settore gestionale                      *
      *                      *-----------------------------------------*
           move      i-ide-set            to   r-ide-set-ges          .
      *                      *-----------------------------------------*
      *                      * Fase gestionale                         *
      *                      *-----------------------------------------*
           move      i-ide-fas            to   r-ide-fas-ges          .
      *                  *---------------------------------------------*
      *                  * Informazioni da preparazione param. stampa  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flags di tipo selezione                 *
      *                      *-----------------------------------------*
           move      w-cnt-stp-tip-sel    to   r-fix-tip-sel          .
      *                      *-----------------------------------------*
      *                      * Codice stampante                        *
      *                      *-----------------------------------------*
           move      w-cnt-stp-cod-stp    to   r-fix-cod-stp          .
      *                      *-----------------------------------------*
      *                      * Tipo di stampa                          *
      *                      *-----------------------------------------*
           move      w-cnt-stp-tip-sta    to   r-fix-tip-sta          .
      *                      *-----------------------------------------*
      *                      * Codice modulo                           *
      *                      *-----------------------------------------*
           move      w-cnt-stp-cod-mod    to   r-fix-cod-mod          .
      *                      *-----------------------------------------*
      *                      * Tipo modulo                             *
      *                      *-----------------------------------------*
           move      w-cnt-stp-tip-mod    to   r-fix-tip-mod          .
      *                      *-----------------------------------------*
      *                      * Ampiezza linea di stampa in caratteri   *
      *                      *-----------------------------------------*
           move      w-cnt-stp-amp-lin    to   r-fix-amp-lin          .
      *                      *-----------------------------------------*
      *                      * Top margin in linee                     *
      *                      *-----------------------------------------*
           move      w-cnt-stp-top-lin    to   r-fix-top-lin          .
      *                      *-----------------------------------------*
      *                      * Numero linee di stampa minimo           *
      *                      *-----------------------------------------*
           move      w-cnt-stp-lin-min    to   r-fix-lin-min          .
      *                      *-----------------------------------------*
      *                      * Bottom margin in linee                  *
      *                      *-----------------------------------------*
           move      w-cnt-stp-bot-lin    to   r-fix-bot-lin          .
      *                      *-----------------------------------------*
      *                      * Ampiezza caratteri                      *
      *                      *-----------------------------------------*
           move      w-cnt-stp-amp-car    to   r-fix-amp-car          .
      *                      *-----------------------------------------*
      *                      * Altezza interlinea                      *
      *                      *-----------------------------------------*
           move      w-cnt-stp-alt-int    to   r-fix-alt-int          .
      *                      *-----------------------------------------*
      *                      * Area riservata per espansioni future    *
      *                      *-----------------------------------------*
           move      w-cnt-stp-esp-fut    to   r-fix-esp-fut          .
      *                      *-----------------------------------------*
      *                      * Area riservata per funzioni speciali    *
      *                      *-----------------------------------------*
           move      w-cnt-stp-fnz-spc    to   r-fix-fnz-spc          .
      *              *-------------------------------------------------*
      *              * Richiamo modulo di selezione stampa             *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/mpslct"
                                         using r                      .
           cancel    "swd/mod/prg/obj/mpslct"                         .
      *              *-------------------------------------------------*
      *              * Status di uscita                                *
      *              *-------------------------------------------------*
           if        r-rsc                not  = spaces
                     move  "#"            to   w-cnt-sel-prm-stp      .
       sel-prm-stp-999.
           exit.

      *    *===========================================================*
      *    * Programma di esecuzione in background                     *
      *    *-----------------------------------------------------------*
       exe-pgm-bkg-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione rullino messaggi di background *
      *              *-------------------------------------------------*
           move      "OB"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Se errore : uscita                          *
      *                  *---------------------------------------------*
            if       m-rsc                not  = spaces
                     go to exe-pgm-bkg-900.
      *              *-------------------------------------------------*
      *              * Scrittura record richieste per background       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio scrittura record richieste           *
      *                  *---------------------------------------------*
           move      "OO"                 to   b-ope                  .
           move      "B"                  to   b-tfe                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
      *                      *-----------------------------------------*
      *                      * Se errore : uscita                      *
      *                      *-----------------------------------------*
            if       b-rsc                not  = spaces
                     go to exe-pgm-bkg-900.
      *                  *---------------------------------------------*
      *                  * Estrazione segmenti da 255  bytes da record *
      *                  * richieste                                   *
      *                  *---------------------------------------------*
           move      1                    to   w-stu-rrr-pnt-stu      .
       exe-pgm-bkg-200.
           move      spaces               to   w-stu-rrr-255-byt      .
           move      w-stu-rrr-pnt-stu    to   w-stu-rrr-sav-pnt      .
           unstring  rr                   into w-stu-rrr-255-byt
                                  with pointer w-stu-rrr-pnt-stu      .
           move      w-stu-rrr-255-byt    to   b-chr                  .
           if        w-stu-rrr-pnt-stu    =    w-stu-rrr-sav-pnt
                     go to exe-pgm-bkg-400.
           move      "PT"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
           go to     exe-pgm-bkg-200.
       exe-pgm-bkg-400.
      *                  *---------------------------------------------*
      *                  * Fine scrittura record richieste             *
      *                  *---------------------------------------------*
           move      "CL"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
      *                      *-----------------------------------------*
      *                      * Se errore : uscita                      *
      *                      *-----------------------------------------*
           if        b-rsc                not  = spaces
                     go to exe-pgm-bkg-900.
      *              *-------------------------------------------------*
      *              * Lancio del programma di esecuzione  background  *
      *              * tramite chiamata al modulo di segreteria        *
      *              *-------------------------------------------------*
           move      "B+"                 to   s-ope                  .
           move      i-exe-pro            to   s-npb                  .
           move      i-exe-pat            to   s-pmo                  .
           move      i-ide-des            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       exe-pgm-bkg-900.
      *              *-------------------------------------------------*
      *              * Cancel del modulo "mbckgr"                      *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mbckgr"                         .
      *              *-------------------------------------------------*
      *              * Cancel del modulo "mmessg"                      *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mmessg"                         .
       exe-pgm-bkg-999.
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
      *              * Estrazione variabili di i.p.c. per eventuale    *
      *              * richiesta di stampa scheda da programma di ges- *
      *              * tione                                           *
      *              *-------------------------------------------------*
           perform   ipc-dlp-tdi-000      thru ipc-dlp-tdi-999        .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Preparazione tipo funzionamento programma                 *
      *    *-----------------------------------------------------------*
       pre-tip-fun-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione delle variabili di i.p.c.*
      *              * lette dal livello precedente                    *
      *              *-------------------------------------------------*
           if        w-ipc-dlp-stp-fnt    =    zero
                     go to pre-tip-fun-200.
       pre-tip-fun-100.
      *              *-------------------------------------------------*
      *              * Se variabile di i.p.c. per codice fornitore     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * No richieste ad utente                      *
      *                  *---------------------------------------------*
           move      "N"                  to   w-cnt-fun-snx-ric      .
      *                  *---------------------------------------------*
      *                  * Si richiesta di selezione stampa            *
      *                  *---------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-stp      .
       pre-tip-fun-120.
      *                  *---------------------------------------------*
      *                  * Preparazione parametri                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo ordinamento stampa : per fornitore *
      *                      *-----------------------------------------*
           move      01                   to   rr-tip-ors             .
      *                      *-----------------------------------------*
      *                      * Tipo ordinamento anagrafica fornitori : *
      *                      * per codice                              *
      *                      *-----------------------------------------*
           move      02                   to   rr-ord-ana-fnt         .
      *                      *-----------------------------------------*
      *                      * Tipo ordinamento anagrafica zone : a    *
      *                      * zero                                    *
      *                      *-----------------------------------------*
           move      zero                 to   rr-ord-ana-zon         .
      *                      *-----------------------------------------*
      *                      * Tipo ordinamento anagrafica categorie : *
      *                      * a zero                                  *
      *                      *-----------------------------------------*
           move      zero                 to   rr-ord-ana-cat         .
      *                      *-----------------------------------------*
      *                      * Tipo ordinamento anagrafica codici sta- *
      *                      * tistici : a zero                        *
      *                      *-----------------------------------------*
           move      zero                 to   rr-ord-ana-stt         .
      *                      *-----------------------------------------*
      *                      * Tipo ordinamento anagrafica nazioni : a *
      *                      * zero                                    *
      *                      *-----------------------------------------*
           move      zero                 to   rr-ord-ana-naz         .
      *                      *-----------------------------------------*
      *                      * Tipo ordinamento anagrafica regioni : a *
      *                      * zero                                    *
      *                      *-----------------------------------------*
           move      zero                 to   rr-ord-ana-rgn         .
      *                      *-----------------------------------------*
      *                      * Tipo ordinamento anagrafica regioni : a *
      *                      * zero                                    *
      *                      *-----------------------------------------*
           move      zero                 to   rr-ord-ana-prv         .
      *                      *-----------------------------------------*
      *                      * Tipo ordinamento anagrafica comuni : a  *
      *                      * zero                                    *
      *                      *-----------------------------------------*
           move      zero                 to   rr-ord-ana-cmn         .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafiche fornitori      *
      *                      *-----------------------------------------*
           move      spaces               to   rr-sel-fnt-rsi         .
           move      all "z"              to   rr-sel-fnt-rsf         .
           move      w-ipc-dlp-stp-fnt    to   rr-sel-fnt-coi         .
           move      w-ipc-dlp-stp-fnt    to   rr-sel-fnt-cof         .
           move      spaces               to   rr-sel-fnt-mni         .
           move      all "z"              to   rr-sel-fnt-mnf         .
           move      w-ipc-dlp-stp-dpz    to   rr-sel-fnt-dpz         .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafiche zone           *
      *                      *-----------------------------------------*
           move      spaces               to   rr-sel-zon-dei         .
           move      all "z"              to   rr-sel-zon-def         .
           move      zero                 to   rr-sel-zon-coi         .
           move      99999                to   rr-sel-zon-cof         .
           move      spaces               to   rr-sel-zon-mni         .
           move      all "z"              to   rr-sel-zon-mnf         .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafiche categorie      *
      *                      *-----------------------------------------*
           move      spaces               to   rr-sel-cat-dei         .
           move      all "z"              to   rr-sel-cat-def         .
           move      zero                 to   rr-sel-cat-coi         .
           move      99999                to   rr-sel-cat-cof         .
           move      spaces               to   rr-sel-cat-mni         .
           move      all "z"              to   rr-sel-cat-mnf         .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafiche codici stati-  *
      *                      * stici                                   *
      *                      *-----------------------------------------*
           move      spaces               to   rr-sel-stt-dei         .
           move      all "z"              to   rr-sel-stt-def         .
           move      zero                 to   rr-sel-stt-coi         .
           move      99999                to   rr-sel-stt-cof         .
           move      spaces               to   rr-sel-stt-mni         .
           move      all "z"              to   rr-sel-stt-mnf         .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafiche nazioni        *
      *                      *-----------------------------------------*
           move      spaces               to   rr-sel-naz-dei         .
           move      all "z"              to   rr-sel-naz-def         .
           move      spaces               to   rr-sel-naz-coi         .
           move      all "z"              to   rr-sel-naz-cof         .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafiche regioni        *
      *                      *-----------------------------------------*
           move      spaces               to   rr-sel-rgn-dei         .
           move      all "z"              to   rr-sel-rgn-def         .
           move      spaces               to   rr-sel-rgn-coi         .
           move      all "z"              to   rr-sel-rgn-cof         .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafiche provincie      *
      *                      *-----------------------------------------*
           move      spaces               to   rr-sel-prv-dei         .
           move      all "z"              to   rr-sel-prv-def         .
           move      spaces               to   rr-sel-prv-coi         .
           move      all "z"              to   rr-sel-prv-cof         .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafiche comuni         *
      *                      *-----------------------------------------*
           move      spaces               to   rr-sel-cmn-dei         .
           move      all "z"              to   rr-sel-cmn-def         .
           move      spaces               to   rr-sel-cmn-cpi         .
           move      all "z"              to   rr-sel-cmn-cpf         .
      *                      *-----------------------------------------*
      *                      * Tipo ordinamento fornitori a parita' di *
      *                      * anagrafica di ordinamento : a zero      *
      *                      *-----------------------------------------*
           move      zero                 to   rr-tip-ocp             .
      *                      *-----------------------------------------*
      *                      * Tipo di stampa : Scheda completa        *
      *                      *-----------------------------------------*
           move      21                   to   rr-tip-stp             .
      *                      *-----------------------------------------*
      *                      * Tipo etichette                          *
      *                      *-----------------------------------------*
           move      zero                 to   rr-tip-eti             .
      *                      *-----------------------------------------*
      *                      * Voci aggiuntive da stampare : Nessuna   *
      *                      *-----------------------------------------*
           move      spaces               to   rr-vag-dst             .
      *                      *-----------------------------------------*
      *                      * Si/no salto pagina a rottura            *
      *                      *-----------------------------------------*
           move      02                   to   rr-sns-prt             .
      *                      *-----------------------------------------*
      *                      * Selettori supplementari                 *
      *                      *-----------------------------------------*
           move      zero                 to   rr-ssu-cod-zon         .
           move      spaces               to   rr-ssu-cod-zon-des     .
           move      zero                 to   rr-ssu-cod-cat         .
           move      spaces               to   rr-ssu-cod-cat-des     .
           move      zero                 to   rr-ssu-cod-stt         .
           move      spaces               to   rr-ssu-cod-stt-des     .
           move      spaces               to   rr-ssu-cod-cap (1)     .
           move      spaces               to   rr-ssu-cod-cap (2)     .
           move      spaces               to   rr-ssu-cod-cap (3)     .
           move      spaces               to   rr-ssu-cod-cap (4)     .
           move      spaces               to   rr-ssu-cod-cap (5)     .
           move      spaces               to   rr-ssu-cod-cap (6)     .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pre-tip-fun-999.
       pre-tip-fun-200.
      *              *-------------------------------------------------*
      *              * Se nessuna variabile di i.p.c.                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si richieste ad utente                      *
      *                  *---------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-ric      .
      *                  *---------------------------------------------*
      *                  * Si richiesta di selezione stampa            *
      *                  *---------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-stp      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pre-tip-fun-999.
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
      *              * Open modulo accettazione codice fornitore com-  *
      *              * merciale                                        *
      *              *-------------------------------------------------*
           perform   cod-mne-dcf-opn-000  thru cod-mne-dcf-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice nostra cassa,   *
      *              * banca, o c/c postale                            *
      *              *-------------------------------------------------*
           perform   cod-des-cbp-opn-000  thru cod-des-cbp-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice zona            *
      *              *-------------------------------------------------*
           perform   cmn-yst-001-opn-000  thru cmn-yst-001-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice categoria       *
      *              *-------------------------------------------------*
           perform   cmn-yst-002-opn-000  thru cmn-yst-002-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice statistico      *
      *              *-------------------------------------------------*
           perform   cmn-yst-003-opn-000  thru cmn-yst-003-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione forma di pagamento     *
      *              *-------------------------------------------------*
           perform   cod-mne-yfp-opn-000  thru cod-mne-yfp-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice nazione         *
      *              *-------------------------------------------------*
           perform   cod-des-naz-opn-000  thru cod-des-naz-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice regione         *
      *              *-------------------------------------------------*
           perform   cod-des-rgn-opn-000  thru cod-des-rgn-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice provincia       *
      *              *-------------------------------------------------*
           perform   cod-des-prv-opn-000  thru cod-des-prv-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione comune                 *
      *              *-------------------------------------------------*
           perform   cod-com-geo-opn-000  thru cod-com-geo-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione frazione               *
      *              *-------------------------------------------------*
           perform   cod-fra-geo-opn-000  thru cod-fra-geo-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione localita'              *
      *              *-------------------------------------------------*
           perform   cod-loc-geo-opn-000  thru cod-loc-geo-opn-999    .
      *              *-------------------------------------------------*
      *              * [cbp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofcbp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cbp                 .
      *              *-------------------------------------------------*
      *              * [yst]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofyst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yst                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice fornitore com- *
      *              * merciale                                        *
      *              *-------------------------------------------------*
           perform   cod-mne-dcf-cls-000  thru cod-mne-dcf-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice nostra cassa,  *
      *              * banca, o c/c postale                            *
      *              *-------------------------------------------------*
           perform   cod-des-cbp-cls-000  thru cod-des-cbp-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice zona           *
      *              *-------------------------------------------------*
           perform   cmn-yst-001-cls-000  thru cmn-yst-001-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice categoria      *
      *              *-------------------------------------------------*
           perform   cmn-yst-002-cls-000  thru cmn-yst-002-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice statistico     *
      *              *-------------------------------------------------*
           perform   cmn-yst-003-cls-000  thru cmn-yst-003-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione forma di pagamento    *
      *              *-------------------------------------------------*
           perform   cod-mne-yfp-cls-000  thru cod-mne-yfp-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice nazione        *
      *              *-------------------------------------------------*
           perform   cod-des-naz-cls-000  thru cod-des-naz-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice regione        *
      *              *-------------------------------------------------*
           perform   cod-des-rgn-cls-000  thru cod-des-rgn-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice provincia      *
      *              *-------------------------------------------------*
           perform   cod-des-prv-cls-000  thru cod-des-prv-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione comune                *
      *              *-------------------------------------------------*
           perform   cod-com-geo-cls-000  thru cod-com-geo-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione frazione              *
      *              *-------------------------------------------------*
           perform   cod-fra-geo-cls-000  thru cod-fra-geo-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione localita'             *
      *              *-------------------------------------------------*
           perform   cod-loc-geo-cls-000  thru cod-loc-geo-cls-999    .
      *              *-------------------------------------------------*
      *              * [cbp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofcbp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cbp                 .
      *              *-------------------------------------------------*
      *              * [yst]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofyst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yst                 .
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
      *                  * Video in 'ON'                               *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Prompts per richieste di selezione          *
      *                  *---------------------------------------------*
           perform   pmt-ric-sel-000      thru pmt-ric-sel-999        .
           move      "#"                  to   w-cnt-sts-pmt-ric      .
       acc-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Accettazioni                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Tipo ordinamento stampa                     *
      *                  *---------------------------------------------*
           perform   acc-tip-ors-000      thru acc-tip-ors-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-150.
      *                  *---------------------------------------------*
      *                  * Tipo ordinamento anagrafica                 *
      *                  *---------------------------------------------*
           perform   acc-ord-ana-000      thru acc-ord-ana-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Selettore su anagrafiche iniziale           *
      *                  *---------------------------------------------*
           perform   acc-sel-ain-000      thru acc-sel-ain-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-150.
       acc-ric-sel-250.
      *                  *---------------------------------------------*
      *                  * Selettore su anagrafiche finale             *
      *                  *---------------------------------------------*
           perform   acc-sel-afi-000      thru acc-sel-afi-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Tipo ordinamento fornitori a parita' di     *
      *                  * anagrafica di ordinamento                   *
      *                  *---------------------------------------------*
           perform   acc-tip-ocp-000      thru acc-tip-ocp-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-250.
       acc-ric-sel-350.
      *                  *---------------------------------------------*
      *                  * Tipo di stampa richiesta                    *
      *                  *---------------------------------------------*
           perform   acc-tip-stp-000      thru acc-tip-stp-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300.
       acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Voci aggiuntive da stampare                 *
      *                  *---------------------------------------------*
           perform   acc-vag-dst-000      thru acc-vag-dst-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-350.
       acc-ric-sel-450.
      *                  *---------------------------------------------*
      *                  * Si/no salto pagina a rottura                *
      *                  *---------------------------------------------*
           perform   acc-sns-prt-000      thru acc-sns-prt-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
       acc-ric-sel-475.
      *                  *---------------------------------------------*
      *                  * Tipo di etichette                           *
      *                  *---------------------------------------------*
           perform   acc-tip-eti-000      thru acc-tip-eti-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-450.
       acc-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Selettori supplementari : codice ns. banca  *
      *                  *---------------------------------------------*
           perform   acc-cod-cbp-000      thru acc-cod-cbp-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-475.
       acc-ric-sel-550.
      *                  *---------------------------------------------*
      *                  * Selettori supplementari : codice zona       *
      *                  *---------------------------------------------*
           perform   acc-cod-zon-000      thru acc-cod-zon-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-500.
       acc-ric-sel-600.
      *                  *---------------------------------------------*
      *                  * Selettori supplementari : codice categoria  *
      *                  *---------------------------------------------*
           perform   acc-cod-cat-000      thru acc-cod-cat-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-550.
       acc-ric-sel-650.
      *                  *---------------------------------------------*
      *                  * Selettori supplementari : codice statistico *
      *                  *---------------------------------------------*
           perform   acc-cod-stt-000      thru acc-cod-stt-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-600.
       acc-ric-sel-700.
      *                  *---------------------------------------------*
      *                  * Selettori supplementari : status commercia- *
      *                  *                           le                *
      *                  *---------------------------------------------*
           perform   acc-sta-tus-000      thru acc-sta-tus-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-650.
       acc-ric-sel-725.
      *                  *---------------------------------------------*
      *                  * Selettori supplementari : codici di avvia-  *
      *                  *                           mento postale     *
      *                  *---------------------------------------------*
           perform   acc-cod-cap-000      thru acc-cod-cap-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-700.
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
           move      "#CNF"               to   v-not                  .
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
      *    * Visualizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       vis-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipi ordinamento anagrafica                 *
      *                  *---------------------------------------------*
           perform   vis-ord-ana-fnt-000  thru vis-ord-ana-fnt-999    .
           perform   vis-ord-ana-zon-000  thru vis-ord-ana-zon-999    .
           perform   vis-ord-ana-cat-000  thru vis-ord-ana-cat-999    .
           perform   vis-ord-ana-stt-000  thru vis-ord-ana-stt-999    .
           perform   vis-ord-ana-naz-000  thru vis-ord-ana-naz-999    .
           perform   vis-ord-ana-rgn-000  thru vis-ord-ana-rgn-999    .
           perform   vis-ord-ana-prv-000  thru vis-ord-ana-prv-999    .
           perform   vis-ord-ana-cmn-000  thru vis-ord-ana-cmn-999    .
      *                  *---------------------------------------------*
      *                  * Selettori su anagrafiche iniziale e finale  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Fornitore                               *
      *                      *-----------------------------------------*
           perform   vis-fnt-rsi-000      thru vis-fnt-rsi-999        .
           perform   vis-fnt-rsf-000      thru vis-fnt-rsf-999        .
           perform   vis-fnt-coi-000      thru vis-fnt-coi-999        .
           perform   vis-fnt-cof-000      thru vis-fnt-cof-999        .
           perform   vis-fnt-mni-000      thru vis-fnt-mni-999        .
           perform   vis-fnt-mnf-000      thru vis-fnt-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Zona                                    *
      *                      *-----------------------------------------*
           perform   vis-zon-dei-000      thru vis-zon-dei-999        .
           perform   vis-zon-def-000      thru vis-zon-def-999        .
           perform   vis-zon-coi-000      thru vis-zon-coi-999        .
           perform   vis-zon-cof-000      thru vis-zon-cof-999        .
           perform   vis-zon-mni-000      thru vis-zon-mni-999        .
           perform   vis-zon-mnf-000      thru vis-zon-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Categorie                               *
      *                      *-----------------------------------------*
           perform   vis-cat-dei-000      thru vis-cat-dei-999        .
           perform   vis-cat-def-000      thru vis-cat-def-999        .
           perform   vis-cat-coi-000      thru vis-cat-coi-999        .
           perform   vis-cat-cof-000      thru vis-cat-cof-999        .
           perform   vis-cat-mni-000      thru vis-cat-mni-999        .
           perform   vis-cat-mnf-000      thru vis-cat-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Codice statistico                       *
      *                      *-----------------------------------------*
           perform   vis-stt-dei-000      thru vis-stt-dei-999        .
           perform   vis-stt-def-000      thru vis-stt-def-999        .
           perform   vis-stt-coi-000      thru vis-stt-coi-999        .
           perform   vis-stt-cof-000      thru vis-stt-cof-999        .
           perform   vis-stt-mni-000      thru vis-stt-mni-999        .
           perform   vis-stt-mnf-000      thru vis-stt-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Codice nazione                          *
      *                      *-----------------------------------------*
           perform   vis-naz-dei-000      thru vis-naz-dei-999        .
           perform   vis-naz-def-000      thru vis-naz-def-999        .
           perform   vis-naz-coi-000      thru vis-naz-coi-999        .
           perform   vis-naz-cof-000      thru vis-naz-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice regione                          *
      *                      *-----------------------------------------*
           perform   vis-rgn-dei-000      thru vis-rgn-dei-999        .
           perform   vis-rgn-def-000      thru vis-rgn-def-999        .
           perform   vis-rgn-coi-000      thru vis-rgn-coi-999        .
           perform   vis-rgn-cof-000      thru vis-rgn-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice provincia                        *
      *                      *-----------------------------------------*
           perform   vis-prv-dei-000      thru vis-prv-dei-999        .
           perform   vis-prv-def-000      thru vis-prv-def-999        .
           perform   vis-prv-coi-000      thru vis-prv-coi-999        .
           perform   vis-prv-cof-000      thru vis-prv-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice comune                           *
      *                      *-----------------------------------------*
           perform   vis-cmn-dei-000      thru vis-cmn-dei-999        .
           perform   vis-cmn-def-000      thru vis-cmn-def-999        .
           perform   vis-cmn-cpi-000      thru vis-cmn-cpi-999        .
           perform   vis-cmn-cpi-000      thru vis-cmn-cpi-999        .
      *                  *---------------------------------------------*
      *                  * Tipo ordinamento fornitori a parita' di     *
      *                  * anagrafica di ordinamento                   *
      *                  *---------------------------------------------*
           perform   vis-tip-ocp-000      thru vis-tip-ocp-999        .
      *                  *---------------------------------------------*
      *                  * Tipo di stampa                              *
      *                  *---------------------------------------------*
           perform   vis-tip-stp-000      thru vis-tip-stp-999        .
      *                  *---------------------------------------------*
      *                  * Voci aggiuntive da stampare                 *
      *                  *---------------------------------------------*
           perform   vis-vag-dst-000      thru vis-vag-dst-999        .
      *                  *---------------------------------------------*
      *                  * Si/no salto pagina a rottura                *
      *                  *---------------------------------------------*
           perform   vis-sns-prt-000      thru vis-sns-prt-999        .
      *                  *---------------------------------------------*
      *                  * Tipo di etichette                           *
      *                  *---------------------------------------------*
           perform   vis-tip-eti-000      thru vis-tip-eti-999        .
      *                  *---------------------------------------------*
      *                  * Selettori supplementari : codice ns. banca  *
      *                  *---------------------------------------------*
           perform   vis-cod-cbp-000      thru vis-cod-cbp-999        .
           perform   vis-cod-cbp-des-000  thru vis-cod-cbp-des-999    .
      *                  *---------------------------------------------*
      *                  * Selettori supplementari : codice zona       *
      *                  *---------------------------------------------*
           perform   vis-cod-zon-000      thru vis-cod-zon-999        .
           perform   vis-cod-zon-des-000  thru vis-cod-zon-des-999    .
      *                  *---------------------------------------------*
      *                  * Selettori supplementari : codice categoria  *
      *                  *---------------------------------------------*
           perform   vis-cod-cat-000      thru vis-cod-cat-999        .
           perform   vis-cod-cat-des-000  thru vis-cod-cat-des-999    .
      *                  *---------------------------------------------*
      *                  * Selettori supplementari : codice statistico *
      *                  *---------------------------------------------*
           perform   vis-cod-stt-000      thru vis-cod-stt-999        .
           perform   vis-cod-stt-des-000  thru vis-cod-stt-des-999    .
      *                  *---------------------------------------------*
      *                  * Selettori supplementari : status commercia- *
      *                  *                           le                *
      *                  *---------------------------------------------*
           perform   vis-sta-tus-000      thru vis-sta-tus-999        .
      *                  *---------------------------------------------*
      *                  * Selettori supplementari : codici di avvia-  *
      *                  *                           mento postale     *
      *                  *---------------------------------------------*
           perform   vis-cod-cap-000      thru vis-cod-cap-999        .
       vis-ric-sel-900.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-ric-sel-999.
       vis-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per richieste di selezione        *
      *    *-----------------------------------------------------------*
       pmt-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Prompts                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo ordinamento stampa                     *
      *                  *---------------------------------------------*
           perform   pmt-tip-ors-000      thru pmt-tip-ors-999        .
      *                  *---------------------------------------------*
      *                  * Tipo ordinamento anagrafica                 *
      *                  *---------------------------------------------*
           perform   pmt-ord-ana-000      thru pmt-ord-ana-999        .
      *                  *---------------------------------------------*
      *                  * Selettori su anagrafiche iniziale           *
      *                  *---------------------------------------------*
           perform   pmt-sel-ain-000      thru pmt-sel-ain-999        .
      *                  *---------------------------------------------*
      *                  * Selettore su anagrafiche finale             *
      *                  *---------------------------------------------*
           perform   pmt-sel-afi-000      thru pmt-sel-afi-999        .
      *                  *---------------------------------------------*
      *                  * Tipo ordinamento fornitori a parita' di     *
      *                  * anagrafica di ordinamento                   *
      *                  *---------------------------------------------*
           perform   pmt-tip-ocp-000      thru pmt-tip-ocp-999        .
      *                  *---------------------------------------------*
      *                  * Tipo di stampa richiesta                    *
      *                  *---------------------------------------------*
           perform   pmt-tip-stp-000      thru pmt-tip-stp-999        .
      *                  *---------------------------------------------*
      *                  * Voci aggiuntive da stampare                 *
      *                  *---------------------------------------------*
           perform   pmt-vag-dst-000      thru pmt-vag-dst-999        .
      *                  *---------------------------------------------*
      *                  * Si/no salto pagina a rottura                *
      *                  *---------------------------------------------*
           perform   pmt-sns-prt-000      thru pmt-sns-prt-999        .
      *                  *---------------------------------------------*
      *                  * Tipo di etichette                           *
      *                  *---------------------------------------------*
           perform   pmt-tip-eti-000      thru pmt-tip-eti-999        .
      *                  *---------------------------------------------*
      *                  * Selettori supplementari : titolo            *
      *                  *---------------------------------------------*
           perform   pmt-sel-sup-000      thru pmt-sel-sup-999        .
      *                  *---------------------------------------------*
      *                  * Selettori supplementari : codice ns. banca  *
      *                  *---------------------------------------------*
           perform   pmt-cod-cbp-000      thru pmt-cod-cbp-999        .
      *                  *---------------------------------------------*
      *                  * Selettori supplementari : codice zona       *
      *                  *---------------------------------------------*
           perform   pmt-cod-zon-000      thru pmt-cod-zon-999        .
      *                  *---------------------------------------------*
      *                  * Selettori supplementari : codice categoria  *
      *                  *---------------------------------------------*
           perform   pmt-cod-cat-000      thru pmt-cod-cat-999        .
      *                  *---------------------------------------------*
      *                  * Selettori supplementari : codice statistico *
      *                  *---------------------------------------------*
           perform   pmt-cod-stt-000      thru pmt-cod-stt-999        .
      *                  *---------------------------------------------*
      *                  * Selettori supplementari : status commercia- *
      *                  *                           le                *
      *                  *---------------------------------------------*
           perform   pmt-sta-tus-000      thru pmt-sta-tus-999        .
      *                  *---------------------------------------------*
      *                  * Selettori supplementari : codici di avvia-  *
      *                  *                           mento postale     *
      *                  *---------------------------------------------*
           perform   pmt-cod-cap-000      thru pmt-cod-cap-999        .
       pmt-ric-sel-900.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-ric-sel-999.
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt per Tipo ordinamento stampa        *
      *    *-----------------------------------------------------------*
       pmt-tip-ors-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-tip-ors-lin    to   v-lin                  .
           move      w-lpt-tip-ors-ppm    to   v-pos                  .
           move      "Tipo ordinamento stampa              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-tip-ors-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt per Tipo ordinamento anagrafica    *
      *    *-----------------------------------------------------------*
       pmt-ord-ana-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del Tipo ordinamento     *
      *              * stampa                                          *
      *              *-------------------------------------------------*
           if        rr-tip-ors           =    01
                     go to pmt-ord-ana-100
           else if   rr-tip-ors           =    03
                     go to pmt-ord-ana-300
           else if   rr-tip-ors           =    04
                     go to pmt-ord-ana-400
           else if   rr-tip-ors           =    05
                     go to pmt-ord-ana-500
           else if   rr-tip-ors           =    07
                     go to pmt-ord-ana-a00
           else if   rr-tip-ors           =    11
                     go to pmt-ord-ana-610
           else if   rr-tip-ors           =    12
                     go to pmt-ord-ana-620
           else if   rr-tip-ors           =    13
                     go to pmt-ord-ana-630
           else if   rr-tip-ors           =    14
                     go to pmt-ord-ana-640
           else if   rr-tip-ors           =    21 or
                     rr-tip-ors           =    22
                     go to pmt-ord-ana-710
           else      go to pmt-ord-ana-800.
       pmt-ord-ana-100.
      *              *-------------------------------------------------*
      *              * Se ordinamento per fornitore                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-ord-ana-lin    to   v-lin                  .
           move      w-lpt-ord-ana-ppm    to   v-pos                  .
           move      "Tipo ordinamento fornitori           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-ord-ana-900.
       pmt-ord-ana-300.
      *              *-------------------------------------------------*
      *              * Se ordinamento per zona                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-ord-ana-lin    to   v-lin                  .
           move      w-lpt-ord-ana-ppm    to   v-pos                  .
           move      "Tipo ordinamento zone                :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-ord-ana-900.
       pmt-ord-ana-400.
      *              *-------------------------------------------------*
      *              * Se ordinamento per categorie                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-ord-ana-lin    to   v-lin                  .
           move      w-lpt-ord-ana-ppm    to   v-pos                  .
           move      "Tipo ordinamento categorie           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-ord-ana-900.
       pmt-ord-ana-500.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici statistici            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-ord-ana-lin    to   v-lin                  .
           move      w-lpt-ord-ana-ppm    to   v-pos                  .
           move      "Tipo ordinamento codici statistici   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-ord-ana-900.
       pmt-ord-ana-a00.
      *              *-------------------------------------------------*
      *              * Se ordinamento per forme di pagamento           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo ordinamento forme di pagamento  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-ord-ana-900.
       pmt-ord-ana-610.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici nazione               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-ord-ana-lin    to   v-lin                  .
           move      w-lpt-ord-ana-ppm    to   v-pos                  .
           move      "Tipo ordinamento codici nazione      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-ord-ana-900.
       pmt-ord-ana-620.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici ragione               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-ord-ana-lin    to   v-lin                  .
           move      w-lpt-ord-ana-ppm    to   v-pos                  .
           move      "Tipo ordinamento codici regione      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-ord-ana-900.
       pmt-ord-ana-630.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici provincia             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-ord-ana-lin    to   v-lin                  .
           move      w-lpt-ord-ana-ppm    to   v-pos                  .
           move      "Tipo ordinamento codici provincia    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-ord-ana-900.
       pmt-ord-ana-640.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici comune                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-ord-ana-lin    to   v-lin                  .
           move      w-lpt-ord-ana-ppm    to   v-pos                  .
           move      "Tipo ordinamento codici comune       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-ord-ana-900.
       pmt-ord-ana-710.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici avviamento postale    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-ord-ana-lin    to   v-lin                  .
           move      w-lpt-ord-ana-ppm    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-ord-ana-900.
       pmt-ord-ana-800.
      *              *-------------------------------------------------*
      *              * Se ordinamento non definito                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-ord-ana-lin    to   v-lin                  .
           move      w-lpt-ord-ana-ppm    to   v-pos                  .
           move      "Tipo ordinamento                     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-ord-ana-900.
       pmt-ord-ana-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-ord-ana-999.
       pmt-ord-ana-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt per Selettore su anagrafiche ini-  *
      *    * ziale                                                     *
      *    *-----------------------------------------------------------*
       pmt-sel-ain-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del Tipo ordinamento     *
      *              * stampa                                          *
      *              *-------------------------------------------------*
           if        rr-tip-ors           =    01
                     go to pmt-sel-ain-100
           else if   rr-tip-ors           =    03
                     go to pmt-sel-ain-300
           else if   rr-tip-ors           =    04
                     go to pmt-sel-ain-400
           else if   rr-tip-ors           =    05
                     go to pmt-sel-ain-500
           else if   rr-tip-ors           =    07
                     go to pmt-sel-ain-a00
           else if   rr-tip-ors           =    11
                     go to pmt-sel-ain-610
           else if   rr-tip-ors           =    12
                     go to pmt-sel-ain-620
           else if   rr-tip-ors           =    13
                     go to pmt-sel-ain-630
           else if   rr-tip-ors           =    14
                     go to pmt-sel-ain-640
           else if   rr-tip-ors           =    21 or
                     rr-tip-ors           =    22
                     go to pmt-sel-ain-710
           else      go to pmt-sel-ain-800.
       pmt-sel-ain-100.
      *              *-------------------------------------------------*
      *              * Se ordinamento per fornitore                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica fornitore                        *
      *                  *---------------------------------------------*
           if        rr-ord-ana-fnt       =    01
                     go to pmt-sel-ain-110
           else if   rr-ord-ana-fnt       =    02
                     go to pmt-sel-ain-120
           else if   rr-ord-ana-fnt       =    03
                     go to pmt-sel-ain-130
           else      go to pmt-sel-ain-190.
       pmt-sel-ain-110.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per ragione sociale      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-ppm    to   v-pos                  .
           move      "Ragione sociale fornitore iniziale   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-ain-900.
       pmt-sel-ain-120.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-ppm    to   v-pos                  .
           move      "Codice fornitore iniziale            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-ain-900.
       pmt-sel-ain-130.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per mnemonico            *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-ppm    to   v-pos                  .
           move      "Mnemonico fornitore iniziale         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-ain-900.
       pmt-sel-ain-190.
      *                  *---------------------------------------------*
      *                  * A ordinamento non definito                  *
      *                  *---------------------------------------------*
           go to     pmt-sel-ain-800.
       pmt-sel-ain-300.
      *              *-------------------------------------------------*
      *              * Se ordinamento per zona                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica zona                             *
      *                  *---------------------------------------------*
           if        rr-ord-ana-zon       =    01
                     go to pmt-sel-ain-310
           else if   rr-ord-ana-zon       =    02
                     go to pmt-sel-ain-320
           else if   rr-ord-ana-zon       =    03
                     go to pmt-sel-ain-330
           else      go to pmt-sel-ain-390.
       pmt-sel-ain-310.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-ppm    to   v-pos                  .
           move      "Codice zona iniziale                 :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-ain-900.
       pmt-sel-ain-320.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-ppm    to   v-pos                  .
           move      "Descrizione zona iniziale            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-ain-900.
       pmt-sel-ain-330.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per mnemonico            *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-ppm    to   v-pos                  .
           move      "Mnemonico zona iniziale              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-ain-900.
       pmt-sel-ain-390.
      *                  *---------------------------------------------*
      *                  * A ordinamento non definito                  *
      *                  *---------------------------------------------*
           go to     pmt-sel-ain-800.
       pmt-sel-ain-400.
      *              *-------------------------------------------------*
      *              * Se ordinamento per categorie                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica categoria                        *
      *                  *---------------------------------------------*
           if        rr-ord-ana-cat       =    01
                     go to pmt-sel-ain-410
           else if   rr-ord-ana-cat       =    02
                     go to pmt-sel-ain-420
           else if   rr-ord-ana-cat       =    03
                     go to pmt-sel-ain-430
           else      go to pmt-sel-ain-490.
       pmt-sel-ain-410.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-ppm    to   v-pos                  .
           move      "Codice categoria iniziale            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-ain-900.
       pmt-sel-ain-420.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-ppm    to   v-pos                  .
           move      "Descrizione categoria iniziale       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-ain-900.
       pmt-sel-ain-430.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per mnemonico            *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-ppm    to   v-pos                  .
           move      "Mnemonico categoria iniziale         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-ain-900.
       pmt-sel-ain-490.
      *                  *---------------------------------------------*
      *                  * A ordinamento non definito                  *
      *                  *---------------------------------------------*
           go to     pmt-sel-ain-800.
       pmt-sel-ain-500.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici statistici            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica codice statistico                *
      *                  *---------------------------------------------*
           if        rr-ord-ana-stt       =    01
                     go to pmt-sel-ain-510
           else if   rr-ord-ana-stt       =    02
                     go to pmt-sel-ain-520
           else if   rr-ord-ana-stt       =    03
                     go to pmt-sel-ain-530
           else      go to pmt-sel-ain-590.
       pmt-sel-ain-510.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-ppm    to   v-pos                  .
           move      "Codice statistico iniziale           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-ain-900.
       pmt-sel-ain-520.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-ppm    to   v-pos                  .
           move      "Descrizione codice stat. iniziale    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-ain-900.
       pmt-sel-ain-530.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per mnemonico            *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-ppm    to   v-pos                  .
           move      "Mnemonico codice statistico iniziale :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-ain-900.
       pmt-sel-ain-590.
      *                  *---------------------------------------------*
      *                  * A ordinamento non definito                  *
      *                  *---------------------------------------------*
           go to     pmt-sel-ain-800.
       pmt-sel-ain-a00.
      *              *-------------------------------------------------*
      *              * Se ordinamento per forme di pagamento           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica forma di pagamento               *
      *                  *---------------------------------------------*
           if        rr-ord-ana-fop       =    01
                     go to pmt-sel-ain-a02
           else if   rr-ord-ana-fop       =    02
                     go to pmt-sel-ain-a04
           else if   rr-ord-ana-fop       =    03
                     go to pmt-sel-ain-a06
           else      go to pmt-sel-ain-a09.
       pmt-sel-ain-a02.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice forma di pagamento iniziale   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-ain-900.
       pmt-sel-ain-a04.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione forma di pagamento iniz. :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-ain-900.
       pmt-sel-ain-a06.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per mnemonico            *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Mnemonico forma di pagamento iniziale:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-ain-900.
       pmt-sel-ain-a09.
      *                  *---------------------------------------------*
      *                  * A ordinamento non definito                  *
      *                  *---------------------------------------------*
           go to     pmt-sel-ain-800.
       pmt-sel-ain-610.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici nazione               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica nazioni                          *
      *                  *---------------------------------------------*
           if        rr-ord-ana-naz       =    01
                     go to pmt-sel-ain-611
           else if   rr-ord-ana-naz       =    02
                     go to pmt-sel-ain-612
           else      go to pmt-sel-ain-615.
       pmt-sel-ain-611.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-ppm    to   v-pos                  .
           move      "Codice nazione iniziale              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-ain-900.
       pmt-sel-ain-612.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-ppm    to   v-pos                  .
           move      "Descrizione nazione iniziale         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-ain-900.
       pmt-sel-ain-615.
      *                  *---------------------------------------------*
      *                  * A ordinamento non definito                  *
      *                  *---------------------------------------------*
           go to     pmt-sel-ain-800.
       pmt-sel-ain-620.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici regione               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica regioni                          *
      *                  *---------------------------------------------*
           if        rr-ord-ana-rgn       =    01
                     go to pmt-sel-ain-621
           else if   rr-ord-ana-rgn       =    02
                     go to pmt-sel-ain-622
           else      go to pmt-sel-ain-625.
       pmt-sel-ain-621.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-ppm    to   v-pos                  .
           move      "Codice regione iniziale              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-ain-900.
       pmt-sel-ain-622.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-ppm    to   v-pos                  .
           move      "Descrizione regione iniziale         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-ain-900.
       pmt-sel-ain-625.
      *                  *---------------------------------------------*
      *                  * A ordinamento non definito                  *
      *                  *---------------------------------------------*
           go to     pmt-sel-ain-800.
       pmt-sel-ain-630.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici provincia             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica provincie                        *
      *                  *---------------------------------------------*
           if        rr-ord-ana-prv       =    01
                     go to pmt-sel-ain-631
           else if   rr-ord-ana-prv       =    02
                     go to pmt-sel-ain-632
           else      go to pmt-sel-ain-635.
       pmt-sel-ain-631.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-ppm    to   v-pos                  .
           move      "Codice provincia iniziale            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-ain-900.
       pmt-sel-ain-632.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-ppm    to   v-pos                  .
           move      "Descrizione provincia iniziale       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-ain-900.
       pmt-sel-ain-635.
      *                  *---------------------------------------------*
      *                  * A ordinamento non definito                  *
      *                  *---------------------------------------------*
           go to     pmt-sel-ain-800.
       pmt-sel-ain-640.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici comune                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica comuni                           *
      *                  *---------------------------------------------*
           if        rr-ord-ana-cmn       =    01
                     go to pmt-sel-ain-641
           else if   rr-ord-ana-cmn       =    02
                     go to pmt-sel-ain-642
           else      go to pmt-sel-ain-645.
       pmt-sel-ain-641.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-ppm    to   v-pos                  .
           move      "Descrizione comune iniziale          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-ain-900.
       pmt-sel-ain-642.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per C.a.p.               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-ppm    to   v-pos                  .
           move      "C.a.p. del comune iniziale           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-ain-900.
       pmt-sel-ain-645.
      *                  *---------------------------------------------*
      *                  * A ordinamento non definito                  *
      *                  *---------------------------------------------*
           go to     pmt-sel-ain-800.
       pmt-sel-ain-710.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici di avviamento postale *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt a spaces                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-ppm    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-sel-ain-900.
       pmt-sel-ain-800.
      *              *-------------------------------------------------*
      *              * Se ordinamento non definito                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-ppm    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-sel-ain-900.
       pmt-sel-ain-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-sel-ain-999.
       pmt-sel-ain-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt per Selettore su anagrafiche fina- *
      *    * le                                                        *
      *    *-----------------------------------------------------------*
       pmt-sel-afi-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del Tipo ordinamento     *
      *              * stampa                                          *
      *              *-------------------------------------------------*
           if        rr-tip-ors           =    01
                     go to pmt-sel-afi-100
           else if   rr-tip-ors           =    03
                     go to pmt-sel-afi-300
           else if   rr-tip-ors           =    04
                     go to pmt-sel-afi-400
           else if   rr-tip-ors           =    05
                     go to pmt-sel-afi-500
           else if   rr-tip-ors           =    07
                     go to pmt-sel-afi-a00
           else if   rr-tip-ors           =    11
                     go to pmt-sel-afi-610
           else if   rr-tip-ors           =    12
                     go to pmt-sel-afi-620
           else if   rr-tip-ors           =    13
                     go to pmt-sel-afi-630
           else if   rr-tip-ors           =    14
                     go to pmt-sel-afi-640
           else if   rr-tip-ors           =    21 or
                     rr-tip-ors           =    22
                     go to pmt-sel-afi-710
           else      go to pmt-sel-afi-800.
       pmt-sel-afi-100.
      *              *-------------------------------------------------*
      *              * Se ordinamento per fornitore                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica fornitore                        *
      *                  *---------------------------------------------*
           if        rr-ord-ana-fnt       =    01
                     go to pmt-sel-afi-110
           else if   rr-ord-ana-fnt       =    02
                     go to pmt-sel-afi-120
           else if   rr-ord-ana-fnt       =    03
                     go to pmt-sel-afi-130
           else      go to pmt-sel-afi-190.
       pmt-sel-afi-110.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per ragione sociale      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-ppm    to   v-pos                  .
           move      "Ragione sociale fornitore finale     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-afi-900.
       pmt-sel-afi-120.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-ppm    to   v-pos                  .
           move      "Codice fornitore finale              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-afi-900.
       pmt-sel-afi-130.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per mnemonico            *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-ppm    to   v-pos                  .
           move      "Mnemonico fornitore finale           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-afi-900.
       pmt-sel-afi-190.
      *                  *---------------------------------------------*
      *                  * A ordinamento non definito                  *
      *                  *---------------------------------------------*
           go to     pmt-sel-afi-800.
       pmt-sel-afi-300.
      *              *-------------------------------------------------*
      *              * Se ordinamento per zona                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica zona                             *
      *                  *---------------------------------------------*
           if        rr-ord-ana-zon       =    01
                     go to pmt-sel-afi-310
           else if   rr-ord-ana-zon       =    02
                     go to pmt-sel-afi-320
           else if   rr-ord-ana-zon       =    03
                     go to pmt-sel-afi-330
           else      go to pmt-sel-afi-390.
       pmt-sel-afi-310.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-ppm    to   v-pos                  .
           move      "Codice zona finale                   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-afi-900.
       pmt-sel-afi-320.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-ppm    to   v-pos                  .
           move      "Descrizione zona finale              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-afi-900.
       pmt-sel-afi-330.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per mnemonico            *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-ppm    to   v-pos                  .
           move      "Mnemonico zona finale                :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-afi-900.
       pmt-sel-afi-390.
      *                  *---------------------------------------------*
      *                  * A ordinamento non definito                  *
      *                  *---------------------------------------------*
           go to     pmt-sel-afi-800.
       pmt-sel-afi-400.
      *              *-------------------------------------------------*
      *              * Se ordinamento per categorie                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica categoria                        *
      *                  *---------------------------------------------*
           if        rr-ord-ana-cat       =    01
                     go to pmt-sel-afi-410
           else if   rr-ord-ana-cat       =    02
                     go to pmt-sel-afi-420
           else if   rr-ord-ana-cat       =    03
                     go to pmt-sel-afi-430
           else      go to pmt-sel-afi-490.
       pmt-sel-afi-410.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-ppm    to   v-pos                  .
           move      "Codice categoria finale              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-afi-900.
       pmt-sel-afi-420.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-ppm    to   v-pos                  .
           move      "Descrizione categoria finale         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-afi-900.
       pmt-sel-afi-430.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per mnemonico            *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-ppm    to   v-pos                  .
           move      "Mnemonico categoria finale           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-afi-900.
       pmt-sel-afi-490.
      *                  *---------------------------------------------*
      *                  * A ordinamento non definito                  *
      *                  *---------------------------------------------*
           go to     pmt-sel-afi-800.
       pmt-sel-afi-500.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici statistici            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica codice statistico                *
      *                  *---------------------------------------------*
           if        rr-ord-ana-stt       =    01
                     go to pmt-sel-afi-510
           else if   rr-ord-ana-stt       =    02
                     go to pmt-sel-afi-520
           else if   rr-ord-ana-stt       =    03
                     go to pmt-sel-afi-530
           else      go to pmt-sel-afi-590.
       pmt-sel-afi-510.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-ppm    to   v-pos                  .
           move      "Codice statistico finale             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-afi-900.
       pmt-sel-afi-520.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-ppm    to   v-pos                  .
           move      "Descrizione codice stat. finale      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-afi-900.
       pmt-sel-afi-530.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per mnemonico            *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-ppm    to   v-pos                  .
           move      "Mnemonico codice statistico finale   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-afi-900.
       pmt-sel-afi-590.
      *                  *---------------------------------------------*
      *                  * A ordinamento non definito                  *
      *                  *---------------------------------------------*
           go to     pmt-sel-afi-800.
       pmt-sel-afi-a00.
      *              *-------------------------------------------------*
      *              * Se ordinamento per forme di pagamento           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica forme di pagamento               *
      *                  *---------------------------------------------*
           if        rr-ord-ana-fop       =    01
                     go to pmt-sel-afi-a02
           else if   rr-ord-ana-fop       =    02
                     go to pmt-sel-afi-a04
           else if   rr-ord-ana-fop       =    03
                     go to pmt-sel-afi-a06
           else      go to pmt-sel-afi-a09.
       pmt-sel-afi-a02.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice forma di pagamento finale     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-afi-900.
       pmt-sel-afi-a04.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione forma di pagamento fin.  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-afi-900.
       pmt-sel-afi-a06.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per mnemonico            *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Mnemonico forma di pagamento finale  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-afi-900.
       pmt-sel-afi-a09.
      *                  *---------------------------------------------*
      *                  * A ordinamento non definito                  *
      *                  *---------------------------------------------*
           go to     pmt-sel-afi-800.
       pmt-sel-afi-610.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici nazione               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica nazioni                          *
      *                  *---------------------------------------------*
           if        rr-ord-ana-naz       =    01
                     go to pmt-sel-afi-611
           else if   rr-ord-ana-naz       =    02
                     go to pmt-sel-afi-612
           else      go to pmt-sel-afi-615.
       pmt-sel-afi-611.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-ppm    to   v-pos                  .
           move      "Codice nazione finale                :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-afi-900.
       pmt-sel-afi-612.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-ppm    to   v-pos                  .
           move      "Descrizione nazione finale           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-afi-900.
       pmt-sel-afi-615.
      *                  *---------------------------------------------*
      *                  * A ordinamento non definito                  *
      *                  *---------------------------------------------*
           go to     pmt-sel-afi-800.
       pmt-sel-afi-620.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici regione               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica regioni                          *
      *                  *---------------------------------------------*
           if        rr-ord-ana-rgn       =    01
                     go to pmt-sel-afi-621
           else if   rr-ord-ana-rgn       =    02
                     go to pmt-sel-afi-622
           else      go to pmt-sel-afi-625.
       pmt-sel-afi-621.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-ppm    to   v-pos                  .
           move      "Codice regione finale                :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-afi-900.
       pmt-sel-afi-622.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-ppm    to   v-pos                  .
           move      "Descrizione regione finale           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-afi-900.
       pmt-sel-afi-625.
      *                  *---------------------------------------------*
      *                  * A ordinamento non definito                  *
      *                  *---------------------------------------------*
           go to     pmt-sel-afi-800.
       pmt-sel-afi-630.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici provincia             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica provincie                        *
      *                  *---------------------------------------------*
           if        rr-ord-ana-prv       =    01
                     go to pmt-sel-afi-631
           else if   rr-ord-ana-prv       =    02
                     go to pmt-sel-afi-632
           else      go to pmt-sel-afi-635.
       pmt-sel-afi-631.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-ppm    to   v-pos                  .
           move      "Codice provincia finale              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-afi-900.
       pmt-sel-afi-632.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-ppm    to   v-pos                  .
           move      "Descrizione provincia finale         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-afi-900.
       pmt-sel-afi-635.
      *                  *---------------------------------------------*
      *                  * A ordinamento non definito                  *
      *                  *---------------------------------------------*
           go to     pmt-sel-afi-800.
       pmt-sel-afi-640.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici comune                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica comuni                           *
      *                  *---------------------------------------------*
           if        rr-ord-ana-cmn       =    01
                     go to pmt-sel-afi-641
           else if   rr-ord-ana-cmn       =    02
                     go to pmt-sel-afi-642
           else      go to pmt-sel-afi-645.
       pmt-sel-afi-641.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-ppm    to   v-pos                  .
           move      "Descrizione comune finale            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-afi-900.
       pmt-sel-afi-642.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per C.a.p.               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-ppm    to   v-pos                  .
           move      "C.a.p. del comune finale             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pmt-sel-afi-900.
       pmt-sel-afi-645.
      *                  *---------------------------------------------*
      *                  * A ordinamento non definito                  *
      *                  *---------------------------------------------*
           go to     pmt-sel-afi-800.
       pmt-sel-afi-710.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici di avviamento postale *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt a spaces                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-ppm    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-sel-afi-900.
       pmt-sel-afi-800.
      *              *-------------------------------------------------*
      *              * Se ordinamento non definito                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-ppm    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-sel-afi-900.
       pmt-sel-afi-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-sel-afi-999.
       pmt-sel-afi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt per Tipo ordinamento fornitori a   *
      *    * parita' di anagrafica di ordinamento                      *
      *    *-----------------------------------------------------------*
       pmt-tip-ocp-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del Tipo ordinamento     *
      *              * stampa                                          *
      *              *-------------------------------------------------*
           if        rr-tip-ors           =    01
                     go to pmt-tip-ocp-100
           else if   rr-tip-ors           =    03
                     go to pmt-tip-ocp-300
           else if   rr-tip-ors           =    04
                     go to pmt-tip-ocp-400
           else if   rr-tip-ors           =    05
                     go to pmt-tip-ocp-500
           else if   rr-tip-ors           =    07
                     go to pmt-tip-ocp-a00
           else if   rr-tip-ors           =    11
                     go to pmt-tip-ocp-610
           else if   rr-tip-ors           =    12
                     go to pmt-tip-ocp-620
           else if   rr-tip-ors           =    13
                     go to pmt-tip-ocp-630
           else if   rr-tip-ors           =    14
                     go to pmt-tip-ocp-640
           else if   rr-tip-ors           =    21 or
                     rr-tip-ors           =    22
                     go to pmt-tip-ocp-710
           else      go to pmt-tip-ocp-800.
       pmt-tip-ocp-100.
      *              *-------------------------------------------------*
      *              * Se ordinamento per fornitore                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt 1                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-tip-ocp-lin    to   v-lin                  .
           move      w-lpt-tip-ocp-ppm    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Prompt 2                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-tip-ocp-lin    to   v-lin                  .
           add       1                    to   v-lin                  .
           move      w-lpt-tip-ocp-ppm    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-tip-ocp-900.
       pmt-tip-ocp-300.
      *              *-------------------------------------------------*
      *              * Se ordinamento per zona                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt 1                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-tip-ocp-lin    to   v-lin                  .
           move      w-lpt-tip-ocp-ppm    to   v-pos                  .
           move      "Tipo ordinamento fornitori a parita'   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Prompt 2                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-tip-ocp-lin    to   v-lin                  .
           add       1                    to   v-lin                  .
           move      w-lpt-tip-ocp-ppm    to   v-pos                  .
           move      "                   di codice zona     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-tip-ocp-900.
       pmt-tip-ocp-400.
      *              *-------------------------------------------------*
      *              * Se ordinamento per categorie                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt 1                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-tip-ocp-lin    to   v-lin                  .
           move      w-lpt-tip-ocp-ppm    to   v-pos                  .
           move      "Tipo ordinamento fornitori a parita' :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Prompt 2                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-tip-ocp-lin    to   v-lin                  .
           add       1                    to   v-lin                  .
           move      w-lpt-tip-ocp-ppm    to   v-pos                  .
           move      "              di codice categoria     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-tip-ocp-900.
       pmt-tip-ocp-500.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici statistici            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt 1                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-tip-ocp-lin    to   v-lin                  .
           move      w-lpt-tip-ocp-ppm    to   v-pos                  .
           move      "Tipo ordinamento fornitori a parita' :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Prompt 2                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-tip-ocp-lin    to   v-lin                  .
           add       1                    to   v-lin                  .
           move      w-lpt-tip-ocp-ppm    to   v-pos                  .
           move      "             di codice statistico     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-tip-ocp-900.
       pmt-tip-ocp-a00.
      *              *-------------------------------------------------*
      *              * Se ordinamento per forme di pagamento           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt 1                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-tip-ocp-lin    to   v-lin                  .
           move      w-lpt-tip-ocp-ppm    to   v-pos                  .
           move      "Tipo ordinamento fornitori a parita' :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Prompt 2                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-tip-ocp-lin    to   v-lin                  .
           add       1                    to   v-lin                  .
           move      w-lpt-tip-ocp-ppm    to   v-pos                  .
           move      "            di forma di pagamento     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-tip-ocp-900.
       pmt-tip-ocp-610.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici nazione               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt 1                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-tip-ocp-lin    to   v-lin                  .
           move      w-lpt-tip-ocp-ppm    to   v-pos                  .
           move      "Tipo ordinamento fornitori a parita' :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Prompt 2                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-tip-ocp-lin    to   v-lin                  .
           add       1                    to   v-lin                  .
           move      w-lpt-tip-ocp-ppm    to   v-pos                  .
           move      "                di codice nazione     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-tip-ocp-900.
       pmt-tip-ocp-620.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici regione               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt 1                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-tip-ocp-lin    to   v-lin                  .
           move      w-lpt-tip-ocp-ppm    to   v-pos                  .
           move      "Tipo ordinamento fornitori a parita' :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Prompt 2                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-tip-ocp-lin    to   v-lin                  .
           add       1                    to   v-lin                  .
           move      w-lpt-tip-ocp-ppm    to   v-pos                  .
           move      "                di codice regione     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-tip-ocp-900.
       pmt-tip-ocp-630.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici provincia             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt 1                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-tip-ocp-lin    to   v-lin                  .
           move      w-lpt-tip-ocp-ppm    to   v-pos                  .
           move      "Tipo ordinamento fornitori a parita' :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Prompt 2                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-tip-ocp-lin    to   v-lin                  .
           add       1                    to   v-lin                  .
           move      w-lpt-tip-ocp-ppm    to   v-pos                  .
           move      "              di codice provincia     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-tip-ocp-900.
       pmt-tip-ocp-640.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici comune                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt 1                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-tip-ocp-lin    to   v-lin                  .
           move      w-lpt-tip-ocp-ppm    to   v-pos                  .
           move      "Tipo ordinamento fornitori a parita' :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Prompt 2                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-tip-ocp-lin    to   v-lin                  .
           add       1                    to   v-lin                  .
           move      w-lpt-tip-ocp-ppm    to   v-pos                  .
           move      "                 di codice comune     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-tip-ocp-900.
       pmt-tip-ocp-710.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici di avviamento postale *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt 1                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-tip-ocp-lin    to   v-lin                  .
           move      w-lpt-tip-ocp-ppm    to   v-pos                  .
           move      "Tipo ordinamento fornitori a parita' :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Prompt 2                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-tip-ocp-lin    to   v-lin                  .
           add       1                    to   v-lin                  .
           move      w-lpt-tip-ocp-ppm    to   v-pos                  .
           move      "  di codice di avviamento postale     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-tip-ocp-900.
       pmt-tip-ocp-800.
      *              *-------------------------------------------------*
      *              * Se ordinamento non definito                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt 1                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-tip-ocp-lin    to   v-lin                  .
           move      w-lpt-tip-ocp-ppm    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Prompt 2                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-tip-ocp-lin    to   v-lin                  .
           add       1                    to   v-lin                  .
           move      w-lpt-tip-ocp-ppm    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-tip-ocp-900.
       pmt-tip-ocp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-tip-ocp-999.
       pmt-tip-ocp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt per Tipo di stampa richiesta       *
      *    *-----------------------------------------------------------*
       pmt-tip-stp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di elaborazione richiesta       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-tip-stp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt per Voci aggiuntive da stampare    *
      *    *-----------------------------------------------------------*
       pmt-vag-dst-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del Tipo stampa          *
      *              *-------------------------------------------------*
           if        rr-tip-stp           =    11
                     go to pmt-vag-dst-100
           else if   rr-tip-stp           =    51
                     go to pmt-vag-dst-500
           else      go to pmt-vag-dst-800.
       pmt-vag-dst-100.
      *              *-------------------------------------------------*
      *              * Se Tipo stampa 11 : con voci aggiuntive         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-vag-dst-lin    to   v-lin                  .
           move      w-lpt-vag-dst-ppm    to   v-pos                  .
           move      "Voci aggiuntive da stampare          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-vag-dst-900.
       pmt-vag-dst-500.
      *              *-------------------------------------------------*
      *              * Se Tipo stampa 51 : archivio sequenziale        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-vag-dst-lin    to   v-lin                  .
           move      w-lpt-vag-dst-ppm    to   v-pos                  .
           move      "Voci aggiuntive da esportare         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-vag-dst-900.
       pmt-vag-dst-800.
      *              *-------------------------------------------------*
      *              * Per tutti gli altri tipi stampa                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt a spaces                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-vag-dst-lin    to   v-lin                  .
           move      w-lpt-vag-dst-ppm    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-vag-dst-900.
       pmt-vag-dst-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-vag-dst-999.
       pmt-vag-dst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt per Si/no salto pagina a rottura   *
      *    *-----------------------------------------------------------*
       pmt-sns-prt-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del Tipo ordinamento     *
      *              * stampa                                          *
      *              *-------------------------------------------------*
           if        rr-tip-ors           =    01
                     go to pmt-sns-prt-100
           else if   rr-tip-ors           =    03
                     go to pmt-sns-prt-300
           else if   rr-tip-ors           =    04
                     go to pmt-sns-prt-400
           else if   rr-tip-ors           =    05
                     go to pmt-sns-prt-500
           else if   rr-tip-ors           =    07
                     go to pmt-sns-prt-a00
           else if   rr-tip-ors           =    11
                     go to pmt-sns-prt-610
           else if   rr-tip-ors           =    12
                     go to pmt-sns-prt-620
           else if   rr-tip-ors           =    13
                     go to pmt-sns-prt-630
           else if   rr-tip-ors           =    14
                     go to pmt-sns-prt-640
           else if   rr-tip-ors           =    21 or
                     rr-tip-ors           =    22
                     go to pmt-sns-prt-710
           else      go to pmt-sns-prt-800.
       pmt-sns-prt-100.
      *              *-------------------------------------------------*
      *              * Se ordinamento per fornitore                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sns-prt-lin    to   v-lin                  .
           move      w-lpt-sns-prt-ppm    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-sns-prt-900.
       pmt-sns-prt-300.
      *              *-------------------------------------------------*
      *              * Se ordinamento per zona                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sns-prt-lin    to   v-lin                  .
           move      w-lpt-sns-prt-ppm    to   v-pos                  .
           move      "Salto pagina a cambio Zona           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-sns-prt-900.
       pmt-sns-prt-400.
      *              *-------------------------------------------------*
      *              * Se ordinamento per categorie                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sns-prt-lin    to   v-lin                  .
           move      w-lpt-sns-prt-ppm    to   v-pos                  .
           move      "Salto pagina a cambio Categoria      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-sns-prt-900.
       pmt-sns-prt-500.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici statistici            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sns-prt-lin    to   v-lin                  .
           move      w-lpt-sns-prt-ppm    to   v-pos                  .
           move      "Salto pagina a cambio Codice statist.:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-sns-prt-900.
       pmt-sns-prt-a00.
      *              *-------------------------------------------------*
      *              * Se ordinamento per forme di pagamento           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sns-prt-lin    to   v-lin                  .
           move      w-lpt-sns-prt-ppm    to   v-pos                  .
           move      "Salto pagina a cambio Forma di pag.  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-sns-prt-900.
       pmt-sns-prt-610.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici nazione               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sns-prt-lin    to   v-lin                  .
           move      w-lpt-sns-prt-ppm    to   v-pos                  .
           move      "Salto pagina a cambio Codice nazione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-sns-prt-900.
       pmt-sns-prt-620.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici regione               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sns-prt-lin    to   v-lin                  .
           move      w-lpt-sns-prt-ppm    to   v-pos                  .
           move      "Salto pagina a cambio Codice regione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-sns-prt-900.
       pmt-sns-prt-630.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici provincia             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sns-prt-lin    to   v-lin                  .
           move      w-lpt-sns-prt-ppm    to   v-pos                  .
           move      "Salto pagina a cambio Cod. provincia :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-sns-prt-900.
       pmt-sns-prt-640.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici comune                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sns-prt-lin    to   v-lin                  .
           move      w-lpt-sns-prt-ppm    to   v-pos                  .
           move      "Salto pagina a cambio Codice comune  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-sns-prt-900.
       pmt-sns-prt-710.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici avviamento postale    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sns-prt-lin    to   v-lin                  .
           move      w-lpt-sns-prt-ppm    to   v-pos                  .
           move      "Salto pagina a cambio C.a.p.         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-sns-prt-900.
       pmt-sns-prt-800.
      *              *-------------------------------------------------*
      *              * Se ordinamento non definito                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-lpt-sns-prt-lin    to   v-lin                  .
           move      w-lpt-sns-prt-ppm    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-sns-prt-900.
       pmt-sns-prt-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-sns-prt-999.
       pmt-sns-prt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt per Tipo di etichette              *
      *    *-----------------------------------------------------------*
       pmt-tip-eti-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del Tipo stampa          *
      *              *-------------------------------------------------*
           if        rr-tip-stp           =    31 or
                     rr-tip-stp           =    32
                     go to pmt-tip-eti-100
           else      go to pmt-tip-eti-200.
       pmt-tip-eti-100.
      *              *-------------------------------------------------*
      *              * Se Tipo stampa 31 : etichette                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di etichette                    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-tip-eti-900.
       pmt-tip-eti-200.
      *              *-------------------------------------------------*
      *              * Per tutti gli altri tipi stampa                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt a spaces                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-tip-eti-900.
       pmt-tip-eti-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-tip-eti-999.
       pmt-tip-eti-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt per Selettori supplementari :      *
      *    *                                                           *
      *    * Titolo                                                    *
      *    *-----------------------------------------------------------*
       pmt-sel-sup-000.
      *              *-------------------------------------------------*
      *              * Prompt                                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "----------------------------- SELEZIONI ADDIZIONAL
      -              "I ----------------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-sel-sup-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt per Selettori supplementari :      *
      *    *                                                           *
      *    * Codici nostra banca                                       *
      *    *-----------------------------------------------------------*
       pmt-cod-cbp-000.
      *              *-------------------------------------------------*
      *              * Prompt                                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Ns. banca .....:"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-cbp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt per Selettori supplementari :      *
      *    *                                                           *
      *    * Codice zona                                               *
      *    *-----------------------------------------------------------*
       pmt-cod-zon-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del Tipo ordinamento     *
      *              * stampa                                          *
      *              *-------------------------------------------------*
           if        rr-tip-ors           =    03
                     go to pmt-cod-zon-500.
       pmt-cod-zon-100.
      *              *-------------------------------------------------*
      *              * Se ordinamento diverso da Zona                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Zona ..........:"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-cod-zon-900.
       pmt-cod-zon-500.
      *              *-------------------------------------------------*
      *              * Se ordinamento per Zona                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-cod-zon-900.
       pmt-cod-zon-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-cod-zon-999.
       pmt-cod-zon-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt per Selettori supplementari :      *
      *    *                                                           *
      *    * Codice categoria                                          *
      *    *-----------------------------------------------------------*
       pmt-cod-cat-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del Tipo ordinamento     *
      *              * stampa                                          *
      *              *-------------------------------------------------*
           if        rr-tip-ors           =    04
                     go to pmt-cod-cat-500.
       pmt-cod-cat-100.
      *              *-------------------------------------------------*
      *              * Se ordinamento diverso da Categoria             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Categoria .....:"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-cod-cat-900.
       pmt-cod-cat-500.
      *              *-------------------------------------------------*
      *              * Se ordinamento per Categoria                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-cod-cat-900.
       pmt-cod-cat-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-cod-cat-999.
       pmt-cod-cat-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt per Selettori supplementari :      *
      *    *                                                           *
      *    * Codice statistico                                         *
      *    *-----------------------------------------------------------*
       pmt-cod-stt-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del Tipo ordinamento     *
      *              * stampa                                          *
      *              *-------------------------------------------------*
           if        rr-tip-ors           =    05
                     go to pmt-cod-stt-500.
       pmt-cod-stt-100.
      *              *-------------------------------------------------*
      *              * Se ordinamento diverso da Codice statistico     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Statistico ....:"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-cod-stt-900.
       pmt-cod-stt-500.
      *              *-------------------------------------------------*
      *              * Se ordinamento per Codice statistico            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     pmt-cod-stt-900.
       pmt-cod-stt-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-cod-stt-999.
       pmt-cod-stt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt per Selettori supplementari :      *
      *    *                                                           *
      *    * Status commerciale                                        *
      *    *-----------------------------------------------------------*
       pmt-sta-tus-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Status ........:"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-sta-tus-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt per Selettori supplementari :      *
      *    *                                                           *
      *    * Codici di avviamento postale                              *
      *    *-----------------------------------------------------------*
       pmt-cod-cap-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      52                   to   v-pos                  .
           move      "Cap .........:"     to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-cap-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo ordinamento stampa              *
      *    *-----------------------------------------------------------*
       acc-tip-ors-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore attuale                  *
      *                  *---------------------------------------------*
           move      rr-tip-ors           to   w-sav-tip-ors          .
       acc-tip-ors-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ors-lun    to   v-car                  .
           move      w-exp-tip-ors-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-lpt-tip-ors-lin    to   v-lin                  .
           move      w-lpt-tip-ors-pos    to   v-pos                  .
           move      w-exp-tip-ors-tbl    to   v-txt                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
      *
           if        rr-tip-ors           =    01
                     move  01             to   v-num
           else if   rr-tip-ors           =    03
                     move  02             to   v-num
           else if   rr-tip-ors           =    04
                     move  03             to   v-num
           else if   rr-tip-ors           =    05
                     move  04             to   v-num
           else if   rr-tip-ors           =    07
                     move  05             to   v-num
           else if   rr-tip-ors           =    11
                     move  06             to   v-num
           else if   rr-tip-ors           =    12
                     move  07             to   v-num
           else if   rr-tip-ors           =    13
                     move  08             to   v-num
           else if   rr-tip-ors           =    14
                     move  09             to   v-num
           else if   rr-tip-ors           =    21
                     move  10             to   v-num
           else if   rr-tip-ors           =    22
                     move  11             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-ors-999.
       acc-tip-ors-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  01             to   rr-tip-ors
           else if   v-num                =    02
                     move  03             to   rr-tip-ors
           else if   v-num                =    03
                     move  04             to   rr-tip-ors
           else if   v-num                =    04
                     move  05             to   rr-tip-ors
           else if   v-num                =    05
                     move  07             to   rr-tip-ors
           else if   v-num                =    06
                     move  11             to   rr-tip-ors
           else if   v-num                =    07
                     move  12             to   rr-tip-ors
           else if   v-num                =    08
                     move  13             to   rr-tip-ors
           else if   v-num                =    09
                     move  14             to   rr-tip-ors
           else if   v-num                =    10
                     move  21             to   rr-tip-ors
           else if   v-num                =    11
                     move  22             to   rr-tip-ors
           else      move  zero           to   rr-tip-ors             .
       acc-tip-ors-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-tip-ors           =    zero
                     go to acc-tip-ors-100.
       acc-tip-ors-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore variato                      *
      *                  *---------------------------------------------*
           if        rr-tip-ors           =    w-sav-tip-ors
                     go to acc-tip-ors-800.
       acc-tip-ors-620.
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori richieste            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipi ordinamento su anagrafiche         *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-ord-000  thru nor-ric-sel-ord-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica fornitori       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-fnt-000  thru nor-ric-sel-fnt-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica zone            *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-zon-000  thru nor-ric-sel-zon-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica categorie       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-cat-000  thru nor-ric-sel-cat-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica statistici      *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-stt-000  thru nor-ric-sel-stt-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica forme di pagam. *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-fop-000  thru nor-ric-sel-fop-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica nazioni         *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-naz-000  thru nor-ric-sel-naz-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica regioni         *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-rgn-000  thru nor-ric-sel-rgn-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica provincie       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-prv-000  thru nor-ric-sel-prv-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica comuni          *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-cmn-000  thru nor-ric-sel-cmn-999    .
      *                      *-----------------------------------------*
      *                      * Selettori supplementari                 *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-ssu-000  thru nor-ric-sel-ssu-999    .
      *                      *-----------------------------------------*
      *                      * Tipo ordinamento fornitori a parita' di *
      *                      * anagrafica di ordinamento               *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-ocp-000  thru nor-ric-sel-ocp-999    .
      *                      *-----------------------------------------*
      *                      * Si/no salto pagina a rottura            *
      *                      *-----------------------------------------*
           move      zero                 to   rr-sns-prt             .
       acc-tip-ors-640.
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts richieste           *
      *                  *---------------------------------------------*
           perform   pmt-ric-sel-000      thru pmt-ric-sel-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione valori richieste            *
      *                  *---------------------------------------------*
           perform   vis-ric-sel-000      thru vis-ric-sel-999        .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-tip-ors-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-ors-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-ors-100.
       acc-tip-ors-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo ordinamento anagrafica          *
      *    *-----------------------------------------------------------*
       acc-ord-ana-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del Tipo ordinamento     *
      *              * stampa                                          *
      *              *-------------------------------------------------*
           if        rr-tip-ors           =    01
                     go to acc-ord-ana-100
           else if   rr-tip-ors           =    03
                     go to acc-ord-ana-300
           else if   rr-tip-ors           =    04
                     go to acc-ord-ana-400
           else if   rr-tip-ors           =    05
                     go to acc-ord-ana-500
           else if   rr-tip-ors           =    07
                     go to acc-ord-ana-a00
           else if   rr-tip-ors           =    11
                     go to acc-ord-ana-610
           else if   rr-tip-ors           =    12
                     go to acc-ord-ana-620
           else if   rr-tip-ors           =    13
                     go to acc-ord-ana-630
           else if   rr-tip-ors           =    14
                     go to acc-ord-ana-640
           else if   rr-tip-ors           =    21 or
                     rr-tip-ors           =    22
                     go to acc-ord-ana-710
           else      go to acc-ord-ana-800.
       acc-ord-ana-100.
      *              *-------------------------------------------------*
      *              * Se ordinamento per fornitore                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione tipi ordinamento per fornitore *
      *                  *---------------------------------------------*
           perform   acc-ord-ana-fnt-000  thru acc-ord-ana-fnt-999    .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-ord-ana-900.
       acc-ord-ana-300.
      *              *-------------------------------------------------*
      *              * Se ordinamento per zona                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione tipi ordinamento per zona      *
      *                  *---------------------------------------------*
           perform   acc-ord-ana-zon-000  thru acc-ord-ana-zon-999    .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-ord-ana-900.
       acc-ord-ana-400.
      *              *-------------------------------------------------*
      *              * Se ordinamento per categoria                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione tipi ordinamento per categoria *
      *                  *---------------------------------------------*
           perform   acc-ord-ana-cat-000  thru acc-ord-ana-cat-999    .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-ord-ana-900.
       acc-ord-ana-500.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codice statistico            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione tipi ordinamento per codice    *
      *                  * statistico                                  *
      *                  *---------------------------------------------*
           perform   acc-ord-ana-stt-000  thru acc-ord-ana-stt-999    .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-ord-ana-900.
       acc-ord-ana-a00.
      *              *-------------------------------------------------*
      *              * Se ordinamento per forma di pagamento           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione tipi ordinamento per forma di  *
      *                  * pagamento                                   *
      *                  *---------------------------------------------*
           perform   acc-ord-ana-fop-000  thru acc-ord-ana-fop-999    .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-ord-ana-900.
       acc-ord-ana-610.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codice nazione               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione tipi ordinamento per codice    *
      *                  * nazione                                     *
      *                  *---------------------------------------------*
           perform   acc-ord-ana-naz-000  thru acc-ord-ana-naz-999    .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-ord-ana-900.
       acc-ord-ana-620.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codice regione               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione tipi ordinamento per codice    *
      *                  * regione                                     *
      *                  *---------------------------------------------*
           perform   acc-ord-ana-rgn-000  thru acc-ord-ana-rgn-999    .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-ord-ana-900.
       acc-ord-ana-630.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codice provincia             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione tipi ordinamento per codice    *
      *                  * provincia                                   *
      *                  *---------------------------------------------*
           perform   acc-ord-ana-prv-000  thru acc-ord-ana-prv-999    .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-ord-ana-900.
       acc-ord-ana-640.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codice comune                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione tipi ordinamento per codice    *
      *                  * comune                                      *
      *                  *---------------------------------------------*
           perform   acc-ord-ana-cmn-000  thru acc-ord-ana-cmn-999    .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-ord-ana-900.
       acc-ord-ana-710.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici avviamento postale    *
      *              *-------------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ad uscita senza alcuna azione           *
      *                      *-----------------------------------------*
           go to     acc-ord-ana-900.
       acc-ord-ana-800.
      *              *-------------------------------------------------*
      *              * Se ordinamento non definito                     *
      *              *-------------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-ord-ana-900.
       acc-ord-ana-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-ord-ana-999.
       acc-ord-ana-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo ordinamento per fornitore       *
      *    *-----------------------------------------------------------*
       acc-ord-ana-fnt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore attuale                  *
      *                  *---------------------------------------------*
           move      rr-ord-ana-fnt       to   w-sav-ana-fnt          .
       acc-ord-ana-fnt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ana-fnt-lun    to   v-car                  .
           move      w-exp-ana-fnt-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-lpt-ord-ana-lin    to   v-lin                  .
           move      w-lpt-ord-ana-pos    to   v-pos                  .
           move      w-exp-ana-fnt-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-ord-ana-fnt       to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-ord-ana-fnt-999.
       acc-ord-ana-fnt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-ord-ana-fnt         .
       acc-ord-ana-fnt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-ord-ana-fnt       =    zero
                     go to acc-ord-ana-fnt-100.
       acc-ord-ana-fnt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore variato                      *
      *                  *---------------------------------------------*
           if        rr-ord-ana-fnt       =    w-sav-ana-fnt
                     go to acc-ord-ana-fnt-800.
       acc-ord-ana-fnt-620.
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori richieste            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica fornitori       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-fnt-000  thru nor-ric-sel-fnt-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica zone            *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-zon-000  thru nor-ric-sel-zon-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica categorie       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-cat-000  thru nor-ric-sel-cat-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica statistici      *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-stt-000  thru nor-ric-sel-stt-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica nazioni         *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-naz-000  thru nor-ric-sel-naz-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica regioni         *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-rgn-000  thru nor-ric-sel-rgn-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica provincie       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-prv-000  thru nor-ric-sel-prv-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica comuni          *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-cmn-000  thru nor-ric-sel-cmn-999    .
      *                      *-----------------------------------------*
      *                      * Selettori supplementari                 *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-ssu-000  thru nor-ric-sel-ssu-999    .
      *                      *-----------------------------------------*
      *                      * Tipo ordinamento fornitori a parita' di *
      *                      * anagrafica di ordinamento               *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-ocp-000  thru nor-ric-sel-ocp-999    .
      *                      *-----------------------------------------*
      *                      * Si/no salto pagina a rottura            *
      *                      *-----------------------------------------*
           move      zero                 to   rr-sns-prt             .
       acc-ord-ana-fnt-640.
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts richieste           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafiche iniziale       *
      *                      *-----------------------------------------*
           perform   pmt-sel-ain-000      thru pmt-sel-ain-999        .
      *                      *-----------------------------------------*
      *                      * Selettore su anagrafiche finale         *
      *                      *-----------------------------------------*
           perform   pmt-sel-afi-000      thru pmt-sel-afi-999        .
      *                      *-----------------------------------------*
      *                      * Tipo ordinamento fornitori a parita' di *
      *                      * anagrafica di ordinamento               *
      *                      *-----------------------------------------*
           perform   pmt-tip-ocp-000      thru pmt-tip-ocp-999        .
      *                      *-----------------------------------------*
      *                      * Si/no salto pagina a rottura            *
      *                      *-----------------------------------------*
           perform   pmt-sns-prt-000      thru pmt-sns-prt-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione valori richieste            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Fornitore                               *
      *                      *-----------------------------------------*
           perform   vis-fnt-rsi-000      thru vis-fnt-rsi-999        .
           perform   vis-fnt-rsf-000      thru vis-fnt-rsf-999        .
           perform   vis-fnt-coi-000      thru vis-fnt-coi-999        .
           perform   vis-fnt-cof-000      thru vis-fnt-cof-999        .
           perform   vis-fnt-mni-000      thru vis-fnt-mni-999        .
           perform   vis-fnt-mnf-000      thru vis-fnt-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Zona                                    *
      *                      *-----------------------------------------*
           perform   vis-zon-dei-000      thru vis-zon-dei-999        .
           perform   vis-zon-def-000      thru vis-zon-def-999        .
           perform   vis-zon-coi-000      thru vis-zon-coi-999        .
           perform   vis-zon-cof-000      thru vis-zon-cof-999        .
           perform   vis-zon-mni-000      thru vis-zon-mni-999        .
           perform   vis-zon-mnf-000      thru vis-zon-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Categorie                               *
      *                      *-----------------------------------------*
           perform   vis-cat-dei-000      thru vis-cat-dei-999        .
           perform   vis-cat-def-000      thru vis-cat-def-999        .
           perform   vis-cat-coi-000      thru vis-cat-coi-999        .
           perform   vis-cat-cof-000      thru vis-cat-cof-999        .
           perform   vis-cat-mni-000      thru vis-cat-mni-999        .
           perform   vis-cat-mnf-000      thru vis-cat-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Codice statistico                       *
      *                      *-----------------------------------------*
           perform   vis-stt-dei-000      thru vis-stt-dei-999        .
           perform   vis-stt-def-000      thru vis-stt-def-999        .
           perform   vis-stt-coi-000      thru vis-stt-coi-999        .
           perform   vis-stt-cof-000      thru vis-stt-cof-999        .
           perform   vis-stt-mni-000      thru vis-stt-mni-999        .
           perform   vis-stt-mnf-000      thru vis-stt-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Codice nazione                          *
      *                      *-----------------------------------------*
           perform   vis-naz-dei-000      thru vis-naz-dei-999        .
           perform   vis-naz-def-000      thru vis-naz-def-999        .
           perform   vis-naz-coi-000      thru vis-naz-coi-999        .
           perform   vis-naz-cof-000      thru vis-naz-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice regione                          *
      *                      *-----------------------------------------*
           perform   vis-rgn-dei-000      thru vis-rgn-dei-999        .
           perform   vis-rgn-def-000      thru vis-rgn-def-999        .
           perform   vis-rgn-coi-000      thru vis-rgn-coi-999        .
           perform   vis-rgn-cof-000      thru vis-rgn-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice provincia                        *
      *                      *-----------------------------------------*
           perform   vis-prv-dei-000      thru vis-prv-dei-999        .
           perform   vis-prv-def-000      thru vis-prv-def-999        .
           perform   vis-prv-coi-000      thru vis-prv-coi-999        .
           perform   vis-prv-cof-000      thru vis-prv-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice comune                           *
      *                      *-----------------------------------------*
           perform   vis-cmn-dei-000      thru vis-cmn-dei-999        .
           perform   vis-cmn-def-000      thru vis-cmn-def-999        .
           perform   vis-cmn-cpi-000      thru vis-cmn-cpi-999        .
           perform   vis-cmn-cpi-000      thru vis-cmn-cpi-999        .
      *                      *-----------------------------------------*
      *                      * Tipo ordinamento fornitori a parita' di *
      *                      * anagrafica di ordinamento               *
      *                      *-----------------------------------------*
           perform   vis-tip-ocp-000      thru vis-tip-ocp-999        .
      *                      *-----------------------------------------*
      *                      * Si/no salto pagina a rottura            *
      *                      *-----------------------------------------*
           perform   vis-sns-prt-000      thru vis-sns-prt-999        .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-ord-ana-fnt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-ord-ana-fnt-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-ord-ana-fnt-100.
       acc-ord-ana-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo ordinamento per fornitore    *
      *    *-----------------------------------------------------------*
       vis-ord-ana-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ana-fnt-lun    to   v-car                  .
           move      w-exp-ana-fnt-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-lpt-ord-ana-lin    to   v-lin                  .
           move      w-lpt-ord-ana-pos    to   v-pos                  .
           move      w-exp-ana-fnt-tbl    to   v-txt                  .
           move      rr-ord-ana-fnt       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ord-ana-fnt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo ordinamento per zona            *
      *    *-----------------------------------------------------------*
       acc-ord-ana-zon-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore attuale                  *
      *                  *---------------------------------------------*
           move      rr-ord-ana-zon       to   w-sav-ana-zon          .
       acc-ord-ana-zon-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ana-zon-lun    to   v-car                  .
           move      w-exp-ana-zon-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-lpt-ord-ana-lin    to   v-lin                  .
           move      w-lpt-ord-ana-pos    to   v-pos                  .
           move      w-exp-ana-zon-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-ord-ana-zon       to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-ord-ana-zon-999.
       acc-ord-ana-zon-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-ord-ana-zon         .
       acc-ord-ana-zon-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-ord-ana-zon       =    zero
                     go to acc-ord-ana-zon-100.
       acc-ord-ana-zon-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore variato                      *
      *                  *---------------------------------------------*
           if        rr-ord-ana-zon       =    w-sav-ana-zon
                     go to acc-ord-ana-zon-800.
       acc-ord-ana-zon-620.
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori richieste            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica fornitori       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-fnt-000  thru nor-ric-sel-fnt-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica zone            *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-zon-000  thru nor-ric-sel-zon-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica categorie       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-cat-000  thru nor-ric-sel-cat-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica statistici      *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-stt-000  thru nor-ric-sel-stt-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica nazioni         *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-naz-000  thru nor-ric-sel-naz-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica regioni         *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-rgn-000  thru nor-ric-sel-rgn-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica provincie       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-prv-000  thru nor-ric-sel-prv-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica comuni          *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-cmn-000  thru nor-ric-sel-cmn-999    .
      *                      *-----------------------------------------*
      *                      * Selettori supplementari                 *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-ssu-000  thru nor-ric-sel-ssu-999    .
       acc-ord-ana-zon-640.
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts richieste           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafiche iniziale       *
      *                      *-----------------------------------------*
           perform   pmt-sel-ain-000      thru pmt-sel-ain-999        .
      *                      *-----------------------------------------*
      *                      * Selettore su anagrafiche finale         *
      *                      *-----------------------------------------*
           perform   pmt-sel-afi-000      thru pmt-sel-afi-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione valori richieste            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Fornitore                               *
      *                      *-----------------------------------------*
           perform   vis-fnt-rsi-000      thru vis-fnt-rsi-999        .
           perform   vis-fnt-rsf-000      thru vis-fnt-rsf-999        .
           perform   vis-fnt-coi-000      thru vis-fnt-coi-999        .
           perform   vis-fnt-cof-000      thru vis-fnt-cof-999        .
           perform   vis-fnt-mni-000      thru vis-fnt-mni-999        .
           perform   vis-fnt-mnf-000      thru vis-fnt-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Zona                                    *
      *                      *-----------------------------------------*
           perform   vis-zon-dei-000      thru vis-zon-dei-999        .
           perform   vis-zon-def-000      thru vis-zon-def-999        .
           perform   vis-zon-coi-000      thru vis-zon-coi-999        .
           perform   vis-zon-cof-000      thru vis-zon-cof-999        .
           perform   vis-zon-mni-000      thru vis-zon-mni-999        .
           perform   vis-zon-mnf-000      thru vis-zon-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Categorie                               *
      *                      *-----------------------------------------*
           perform   vis-cat-dei-000      thru vis-cat-dei-999        .
           perform   vis-cat-def-000      thru vis-cat-def-999        .
           perform   vis-cat-coi-000      thru vis-cat-coi-999        .
           perform   vis-cat-cof-000      thru vis-cat-cof-999        .
           perform   vis-cat-mni-000      thru vis-cat-mni-999        .
           perform   vis-cat-mnf-000      thru vis-cat-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Codice statistico                       *
      *                      *-----------------------------------------*
           perform   vis-stt-dei-000      thru vis-stt-dei-999        .
           perform   vis-stt-def-000      thru vis-stt-def-999        .
           perform   vis-stt-coi-000      thru vis-stt-coi-999        .
           perform   vis-stt-cof-000      thru vis-stt-cof-999        .
           perform   vis-stt-mni-000      thru vis-stt-mni-999        .
           perform   vis-stt-mnf-000      thru vis-stt-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Codice nazione                          *
      *                      *-----------------------------------------*
           perform   vis-naz-dei-000      thru vis-naz-dei-999        .
           perform   vis-naz-def-000      thru vis-naz-def-999        .
           perform   vis-naz-coi-000      thru vis-naz-coi-999        .
           perform   vis-naz-cof-000      thru vis-naz-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice regione                          *
      *                      *-----------------------------------------*
           perform   vis-rgn-dei-000      thru vis-rgn-dei-999        .
           perform   vis-rgn-def-000      thru vis-rgn-def-999        .
           perform   vis-rgn-coi-000      thru vis-rgn-coi-999        .
           perform   vis-rgn-cof-000      thru vis-rgn-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice provincia                        *
      *                      *-----------------------------------------*
           perform   vis-prv-dei-000      thru vis-prv-dei-999        .
           perform   vis-prv-def-000      thru vis-prv-def-999        .
           perform   vis-prv-coi-000      thru vis-prv-coi-999        .
           perform   vis-prv-cof-000      thru vis-prv-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice comune                           *
      *                      *-----------------------------------------*
           perform   vis-cmn-dei-000      thru vis-cmn-dei-999        .
           perform   vis-cmn-def-000      thru vis-cmn-def-999        .
           perform   vis-cmn-cpi-000      thru vis-cmn-cpi-999        .
           perform   vis-cmn-cpi-000      thru vis-cmn-cpi-999        .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-ord-ana-zon-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-ord-ana-zon-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-ord-ana-zon-100.
       acc-ord-ana-zon-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo ordinamento per zona         *
      *    *-----------------------------------------------------------*
       vis-ord-ana-zon-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ana-zon-lun    to   v-car                  .
           move      w-exp-ana-zon-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-lpt-ord-ana-lin    to   v-lin                  .
           move      w-lpt-ord-ana-pos    to   v-pos                  .
           move      w-exp-ana-zon-tbl    to   v-txt                  .
           move      rr-ord-ana-zon       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ord-ana-zon-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo ordinamento per categoria       *
      *    *-----------------------------------------------------------*
       acc-ord-ana-cat-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore attuale                  *
      *                  *---------------------------------------------*
           move      rr-ord-ana-cat       to   w-sav-ana-cat          .
       acc-ord-ana-cat-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ana-cat-lun    to   v-car                  .
           move      w-exp-ana-cat-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-lpt-ord-ana-lin    to   v-lin                  .
           move      w-lpt-ord-ana-pos    to   v-pos                  .
           move      w-exp-ana-cat-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-ord-ana-cat       to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-ord-ana-cat-999.
       acc-ord-ana-cat-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-ord-ana-cat         .
       acc-ord-ana-cat-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-ord-ana-cat       =    zero
                     go to acc-ord-ana-cat-100.
       acc-ord-ana-cat-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore variato                      *
      *                  *---------------------------------------------*
           if        rr-ord-ana-cat       =    w-sav-ana-cat
                     go to acc-ord-ana-cat-800.
       acc-ord-ana-cat-620.
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori richieste            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica fornitori       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-fnt-000  thru nor-ric-sel-fnt-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica zone            *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-zon-000  thru nor-ric-sel-zon-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica categorie       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-cat-000  thru nor-ric-sel-cat-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica statistici      *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-stt-000  thru nor-ric-sel-stt-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica nazioni         *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-naz-000  thru nor-ric-sel-naz-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica regioni         *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-rgn-000  thru nor-ric-sel-rgn-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica provincie       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-prv-000  thru nor-ric-sel-prv-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica comuni          *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-cmn-000  thru nor-ric-sel-cmn-999    .
      *                      *-----------------------------------------*
      *                      * Selettori supplementari                 *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-ssu-000  thru nor-ric-sel-ssu-999    .
       acc-ord-ana-cat-640.
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts richieste           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafiche iniziale       *
      *                      *-----------------------------------------*
           perform   pmt-sel-ain-000      thru pmt-sel-ain-999        .
      *                      *-----------------------------------------*
      *                      * Selettore su anagrafiche finale         *
      *                      *-----------------------------------------*
           perform   pmt-sel-afi-000      thru pmt-sel-afi-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione valori richieste            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Fornitore                               *
      *                      *-----------------------------------------*
           perform   vis-fnt-rsi-000      thru vis-fnt-rsi-999        .
           perform   vis-fnt-rsf-000      thru vis-fnt-rsf-999        .
           perform   vis-fnt-coi-000      thru vis-fnt-coi-999        .
           perform   vis-fnt-cof-000      thru vis-fnt-cof-999        .
           perform   vis-fnt-mni-000      thru vis-fnt-mni-999        .
           perform   vis-fnt-mnf-000      thru vis-fnt-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Zona                                    *
      *                      *-----------------------------------------*
           perform   vis-zon-dei-000      thru vis-zon-dei-999        .
           perform   vis-zon-def-000      thru vis-zon-def-999        .
           perform   vis-zon-coi-000      thru vis-zon-coi-999        .
           perform   vis-zon-cof-000      thru vis-zon-cof-999        .
           perform   vis-zon-mni-000      thru vis-zon-mni-999        .
           perform   vis-zon-mnf-000      thru vis-zon-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Categorie                               *
      *                      *-----------------------------------------*
           perform   vis-cat-dei-000      thru vis-cat-dei-999        .
           perform   vis-cat-def-000      thru vis-cat-def-999        .
           perform   vis-cat-coi-000      thru vis-cat-coi-999        .
           perform   vis-cat-cof-000      thru vis-cat-cof-999        .
           perform   vis-cat-mni-000      thru vis-cat-mni-999        .
           perform   vis-cat-mnf-000      thru vis-cat-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Codice statistico                       *
      *                      *-----------------------------------------*
           perform   vis-stt-dei-000      thru vis-stt-dei-999        .
           perform   vis-stt-def-000      thru vis-stt-def-999        .
           perform   vis-stt-coi-000      thru vis-stt-coi-999        .
           perform   vis-stt-cof-000      thru vis-stt-cof-999        .
           perform   vis-stt-mni-000      thru vis-stt-mni-999        .
           perform   vis-stt-mnf-000      thru vis-stt-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Codice nazione                          *
      *                      *-----------------------------------------*
           perform   vis-naz-dei-000      thru vis-naz-dei-999        .
           perform   vis-naz-def-000      thru vis-naz-def-999        .
           perform   vis-naz-coi-000      thru vis-naz-coi-999        .
           perform   vis-naz-cof-000      thru vis-naz-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice regione                          *
      *                      *-----------------------------------------*
           perform   vis-rgn-dei-000      thru vis-rgn-dei-999        .
           perform   vis-rgn-def-000      thru vis-rgn-def-999        .
           perform   vis-rgn-coi-000      thru vis-rgn-coi-999        .
           perform   vis-rgn-cof-000      thru vis-rgn-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice provincia                        *
      *                      *-----------------------------------------*
           perform   vis-prv-dei-000      thru vis-prv-dei-999        .
           perform   vis-prv-def-000      thru vis-prv-def-999        .
           perform   vis-prv-coi-000      thru vis-prv-coi-999        .
           perform   vis-prv-cof-000      thru vis-prv-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice comune                           *
      *                      *-----------------------------------------*
           perform   vis-cmn-dei-000      thru vis-cmn-dei-999        .
           perform   vis-cmn-def-000      thru vis-cmn-def-999        .
           perform   vis-cmn-cpi-000      thru vis-cmn-cpi-999        .
           perform   vis-cmn-cpi-000      thru vis-cmn-cpi-999        .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-ord-ana-cat-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-ord-ana-cat-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-ord-ana-cat-100.
       acc-ord-ana-cat-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo ordinamento per categoria    *
      *    *-----------------------------------------------------------*
       vis-ord-ana-cat-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ana-cat-lun    to   v-car                  .
           move      w-exp-ana-cat-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-lpt-ord-ana-lin    to   v-lin                  .
           move      w-lpt-ord-ana-pos    to   v-pos                  .
           move      w-exp-ana-cat-tbl    to   v-txt                  .
           move      rr-ord-ana-cat       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ord-ana-cat-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo ordinamento per codice statis-  *
      *    *                      tico                                 *
      *    *-----------------------------------------------------------*
       acc-ord-ana-stt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore attuale                  *
      *                  *---------------------------------------------*
           move      rr-ord-ana-stt       to   w-sav-ana-stt          .
       acc-ord-ana-stt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ana-stt-lun    to   v-car                  .
           move      w-exp-ana-stt-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-lpt-ord-ana-lin    to   v-lin                  .
           move      w-lpt-ord-ana-pos    to   v-pos                  .
           move      w-exp-ana-stt-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-ord-ana-stt       to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-ord-ana-stt-999.
       acc-ord-ana-stt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-ord-ana-stt         .
       acc-ord-ana-stt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-ord-ana-stt       =    zero
                     go to acc-ord-ana-stt-100.
       acc-ord-ana-stt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore variato                      *
      *                  *---------------------------------------------*
           if        rr-ord-ana-stt       =    w-sav-ana-stt
                     go to acc-ord-ana-stt-800.
       acc-ord-ana-stt-620.
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori richieste            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica fornitori       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-fnt-000  thru nor-ric-sel-fnt-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica zone            *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-zon-000  thru nor-ric-sel-zon-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica categorie       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-cat-000  thru nor-ric-sel-cat-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica statistici      *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-stt-000  thru nor-ric-sel-stt-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica nazioni         *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-naz-000  thru nor-ric-sel-naz-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica regioni         *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-rgn-000  thru nor-ric-sel-rgn-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica provincie       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-prv-000  thru nor-ric-sel-prv-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica comuni          *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-cmn-000  thru nor-ric-sel-cmn-999    .
      *                      *-----------------------------------------*
      *                      * Selettori supplementari                 *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-ssu-000  thru nor-ric-sel-ssu-999    .
       acc-ord-ana-stt-640.
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts richieste           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafiche iniziale       *
      *                      *-----------------------------------------*
           perform   pmt-sel-ain-000      thru pmt-sel-ain-999        .
      *                      *-----------------------------------------*
      *                      * Selettore su anagrafiche finale         *
      *                      *-----------------------------------------*
           perform   pmt-sel-afi-000      thru pmt-sel-afi-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione valori richieste            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Fornitore                               *
      *                      *-----------------------------------------*
           perform   vis-fnt-rsi-000      thru vis-fnt-rsi-999        .
           perform   vis-fnt-rsf-000      thru vis-fnt-rsf-999        .
           perform   vis-fnt-coi-000      thru vis-fnt-coi-999        .
           perform   vis-fnt-cof-000      thru vis-fnt-cof-999        .
           perform   vis-fnt-mni-000      thru vis-fnt-mni-999        .
           perform   vis-fnt-mnf-000      thru vis-fnt-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Zona                                    *
      *                      *-----------------------------------------*
           perform   vis-zon-dei-000      thru vis-zon-dei-999        .
           perform   vis-zon-def-000      thru vis-zon-def-999        .
           perform   vis-zon-coi-000      thru vis-zon-coi-999        .
           perform   vis-zon-cof-000      thru vis-zon-cof-999        .
           perform   vis-zon-mni-000      thru vis-zon-mni-999        .
           perform   vis-zon-mnf-000      thru vis-zon-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Categorie                               *
      *                      *-----------------------------------------*
           perform   vis-cat-dei-000      thru vis-cat-dei-999        .
           perform   vis-cat-def-000      thru vis-cat-def-999        .
           perform   vis-cat-coi-000      thru vis-cat-coi-999        .
           perform   vis-cat-cof-000      thru vis-cat-cof-999        .
           perform   vis-cat-mni-000      thru vis-cat-mni-999        .
           perform   vis-cat-mnf-000      thru vis-cat-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Codice statistico                       *
      *                      *-----------------------------------------*
           perform   vis-stt-dei-000      thru vis-stt-dei-999        .
           perform   vis-stt-def-000      thru vis-stt-def-999        .
           perform   vis-stt-coi-000      thru vis-stt-coi-999        .
           perform   vis-stt-cof-000      thru vis-stt-cof-999        .
           perform   vis-stt-mni-000      thru vis-stt-mni-999        .
           perform   vis-stt-mnf-000      thru vis-stt-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Codice nazione                          *
      *                      *-----------------------------------------*
           perform   vis-naz-dei-000      thru vis-naz-dei-999        .
           perform   vis-naz-def-000      thru vis-naz-def-999        .
           perform   vis-naz-coi-000      thru vis-naz-coi-999        .
           perform   vis-naz-cof-000      thru vis-naz-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice regione                          *
      *                      *-----------------------------------------*
           perform   vis-rgn-dei-000      thru vis-rgn-dei-999        .
           perform   vis-rgn-def-000      thru vis-rgn-def-999        .
           perform   vis-rgn-coi-000      thru vis-rgn-coi-999        .
           perform   vis-rgn-cof-000      thru vis-rgn-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice provincia                        *
      *                      *-----------------------------------------*
           perform   vis-prv-dei-000      thru vis-prv-dei-999        .
           perform   vis-prv-def-000      thru vis-prv-def-999        .
           perform   vis-prv-coi-000      thru vis-prv-coi-999        .
           perform   vis-prv-cof-000      thru vis-prv-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice comune                           *
      *                      *-----------------------------------------*
           perform   vis-cmn-dei-000      thru vis-cmn-dei-999        .
           perform   vis-cmn-def-000      thru vis-cmn-def-999        .
           perform   vis-cmn-cpi-000      thru vis-cmn-cpi-999        .
           perform   vis-cmn-cpi-000      thru vis-cmn-cpi-999        .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-ord-ana-stt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-ord-ana-stt-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-ord-ana-stt-100.
       acc-ord-ana-stt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo ordinamento per codice sta-  *
      *    *                         tistico                           *
      *    *-----------------------------------------------------------*
       vis-ord-ana-stt-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ana-stt-lun    to   v-car                  .
           move      w-exp-ana-stt-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-lpt-ord-ana-lin    to   v-lin                  .
           move      w-lpt-ord-ana-pos    to   v-pos                  .
           move      w-exp-ana-stt-tbl    to   v-txt                  .
           move      rr-ord-ana-stt       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ord-ana-stt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo ordinamento per forma di paga-  *
      *    *                      mento                                *
      *    *-----------------------------------------------------------*
       acc-ord-ana-fop-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore attuale                  *
      *                  *---------------------------------------------*
           move      rr-ord-ana-fop       to   w-sav-ana-fop          .
       acc-ord-ana-fop-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ana-fop-lun    to   v-car                  .
           move      w-exp-ana-fop-num    to   v-ldt                  .
           move      "CDM#"               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      05                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      w-exp-ana-fop-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-ord-ana-fop       to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-ord-ana-fop-999.
       acc-ord-ana-fop-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-ord-ana-fop         .
       acc-ord-ana-fop-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-ord-ana-fop       =    zero
                     go to acc-ord-ana-fop-100.
       acc-ord-ana-fop-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore variato                      *
      *                  *---------------------------------------------*
           if        rr-ord-ana-fop       =    w-sav-ana-fop
                     go to acc-ord-ana-fop-800.
       acc-ord-ana-fop-620.
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori richieste            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica fornitori       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-fnt-000  thru nor-ric-sel-fnt-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica zone            *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-zon-000  thru nor-ric-sel-zon-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica categorie       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-cat-000  thru nor-ric-sel-cat-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica statistici      *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-stt-000  thru nor-ric-sel-stt-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica forme di pagam. *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-fop-000  thru nor-ric-sel-fop-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica nazioni         *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-naz-000  thru nor-ric-sel-naz-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica regioni         *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-rgn-000  thru nor-ric-sel-rgn-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica provincie       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-prv-000  thru nor-ric-sel-prv-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica comuni          *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-cmn-000  thru nor-ric-sel-cmn-999    .
      *                      *-----------------------------------------*
      *                      * Selettori supplementari                 *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-ssu-000  thru nor-ric-sel-ssu-999    .
       acc-ord-ana-fop-640.
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts richieste           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafiche iniziale       *
      *                      *-----------------------------------------*
           perform   pmt-sel-ain-000      thru pmt-sel-ain-999        .
      *                      *-----------------------------------------*
      *                      * Selettore su anagrafiche finale         *
      *                      *-----------------------------------------*
           perform   pmt-sel-afi-000      thru pmt-sel-afi-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione valori richieste            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Fornitore                               *
      *                      *-----------------------------------------*
           perform   vis-fnt-rsi-000      thru vis-fnt-rsi-999        .
           perform   vis-fnt-rsf-000      thru vis-fnt-rsf-999        .
           perform   vis-fnt-coi-000      thru vis-fnt-coi-999        .
           perform   vis-fnt-cof-000      thru vis-fnt-cof-999        .
           perform   vis-fnt-mni-000      thru vis-fnt-mni-999        .
           perform   vis-fnt-mnf-000      thru vis-fnt-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Zona                                    *
      *                      *-----------------------------------------*
           perform   vis-zon-dei-000      thru vis-zon-dei-999        .
           perform   vis-zon-def-000      thru vis-zon-def-999        .
           perform   vis-zon-coi-000      thru vis-zon-coi-999        .
           perform   vis-zon-cof-000      thru vis-zon-cof-999        .
           perform   vis-zon-mni-000      thru vis-zon-mni-999        .
           perform   vis-zon-mnf-000      thru vis-zon-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Categorie                               *
      *                      *-----------------------------------------*
           perform   vis-cat-dei-000      thru vis-cat-dei-999        .
           perform   vis-cat-def-000      thru vis-cat-def-999        .
           perform   vis-cat-coi-000      thru vis-cat-coi-999        .
           perform   vis-cat-cof-000      thru vis-cat-cof-999        .
           perform   vis-cat-mni-000      thru vis-cat-mni-999        .
           perform   vis-cat-mnf-000      thru vis-cat-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Codice statistico                       *
      *                      *-----------------------------------------*
           perform   vis-stt-dei-000      thru vis-stt-dei-999        .
           perform   vis-stt-def-000      thru vis-stt-def-999        .
           perform   vis-stt-coi-000      thru vis-stt-coi-999        .
           perform   vis-stt-cof-000      thru vis-stt-cof-999        .
           perform   vis-stt-mni-000      thru vis-stt-mni-999        .
           perform   vis-stt-mnf-000      thru vis-stt-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Forma di pagamento                      *
      *                      *-----------------------------------------*
           perform   vis-fop-dei-000      thru vis-fop-dei-999        .
           perform   vis-fop-def-000      thru vis-fop-def-999        .
           perform   vis-fop-coi-000      thru vis-fop-coi-999        .
           perform   vis-fop-cof-000      thru vis-fop-cof-999        .
           perform   vis-fop-mni-000      thru vis-fop-mni-999        .
           perform   vis-fop-mnf-000      thru vis-fop-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Codice nazione                          *
      *                      *-----------------------------------------*
           perform   vis-naz-dei-000      thru vis-naz-dei-999        .
           perform   vis-naz-def-000      thru vis-naz-def-999        .
           perform   vis-naz-coi-000      thru vis-naz-coi-999        .
           perform   vis-naz-cof-000      thru vis-naz-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice regione                          *
      *                      *-----------------------------------------*
           perform   vis-rgn-dei-000      thru vis-rgn-dei-999        .
           perform   vis-rgn-def-000      thru vis-rgn-def-999        .
           perform   vis-rgn-coi-000      thru vis-rgn-coi-999        .
           perform   vis-rgn-cof-000      thru vis-rgn-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice provincia                        *
      *                      *-----------------------------------------*
           perform   vis-prv-dei-000      thru vis-prv-dei-999        .
           perform   vis-prv-def-000      thru vis-prv-def-999        .
           perform   vis-prv-coi-000      thru vis-prv-coi-999        .
           perform   vis-prv-cof-000      thru vis-prv-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice comune                           *
      *                      *-----------------------------------------*
           perform   vis-cmn-dei-000      thru vis-cmn-dei-999        .
           perform   vis-cmn-def-000      thru vis-cmn-def-999        .
           perform   vis-cmn-cpi-000      thru vis-cmn-cpi-999        .
           perform   vis-cmn-cpi-000      thru vis-cmn-cpi-999        .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-ord-ana-fop-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-ord-ana-fop-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-ord-ana-fop-100.
       acc-ord-ana-fop-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo ordinamento per forma di     *
      *    *                         pagamento                         *
      *    *-----------------------------------------------------------*
       vis-ord-ana-fop-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ana-fop-lun    to   v-car                  .
           move      w-exp-ana-fop-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      05                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      w-exp-ana-fop-tbl    to   v-txt                  .
           move      rr-ord-ana-fop       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ord-ana-fop-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo ordinamento per codice nazione  *
      *    *-----------------------------------------------------------*
       acc-ord-ana-naz-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore attuale                  *
      *                  *---------------------------------------------*
           move      rr-ord-ana-naz       to   w-sav-ana-naz          .
       acc-ord-ana-naz-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ana-naz-lun    to   v-car                  .
           move      w-exp-ana-naz-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-lpt-ord-ana-lin    to   v-lin                  .
           move      w-lpt-ord-ana-pos    to   v-pos                  .
           move      w-exp-ana-naz-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-ord-ana-naz       to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-ord-ana-naz-999.
       acc-ord-ana-naz-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-ord-ana-naz         .
       acc-ord-ana-naz-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-ord-ana-naz       =    zero
                     go to acc-ord-ana-naz-100.
       acc-ord-ana-naz-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore variato                      *
      *                  *---------------------------------------------*
           if        rr-ord-ana-naz       =    w-sav-ana-naz
                     go to acc-ord-ana-naz-800.
       acc-ord-ana-naz-620.
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori richieste            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica fornitori       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-fnt-000  thru nor-ric-sel-fnt-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica zone            *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-zon-000  thru nor-ric-sel-zon-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica categorie       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-cat-000  thru nor-ric-sel-cat-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica statistici      *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-stt-000  thru nor-ric-sel-stt-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica nazioni         *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-naz-000  thru nor-ric-sel-naz-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica regioni         *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-rgn-000  thru nor-ric-sel-rgn-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica provincie       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-prv-000  thru nor-ric-sel-prv-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica comuni          *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-cmn-000  thru nor-ric-sel-cmn-999    .
      *                      *-----------------------------------------*
      *                      * Selettori supplementari                 *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-ssu-000  thru nor-ric-sel-ssu-999    .
       acc-ord-ana-naz-640.
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts richieste           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafiche iniziale       *
      *                      *-----------------------------------------*
           perform   pmt-sel-ain-000      thru pmt-sel-ain-999        .
      *                      *-----------------------------------------*
      *                      * Selettore su anagrafiche finale         *
      *                      *-----------------------------------------*
           perform   pmt-sel-afi-000      thru pmt-sel-afi-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione valori richieste            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Fornitore                               *
      *                      *-----------------------------------------*
           perform   vis-fnt-rsi-000      thru vis-fnt-rsi-999        .
           perform   vis-fnt-rsf-000      thru vis-fnt-rsf-999        .
           perform   vis-fnt-coi-000      thru vis-fnt-coi-999        .
           perform   vis-fnt-cof-000      thru vis-fnt-cof-999        .
           perform   vis-fnt-mni-000      thru vis-fnt-mni-999        .
           perform   vis-fnt-mnf-000      thru vis-fnt-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Zona                                    *
      *                      *-----------------------------------------*
           perform   vis-zon-dei-000      thru vis-zon-dei-999        .
           perform   vis-zon-def-000      thru vis-zon-def-999        .
           perform   vis-zon-coi-000      thru vis-zon-coi-999        .
           perform   vis-zon-cof-000      thru vis-zon-cof-999        .
           perform   vis-zon-mni-000      thru vis-zon-mni-999        .
           perform   vis-zon-mnf-000      thru vis-zon-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Categorie                               *
      *                      *-----------------------------------------*
           perform   vis-cat-dei-000      thru vis-cat-dei-999        .
           perform   vis-cat-def-000      thru vis-cat-def-999        .
           perform   vis-cat-coi-000      thru vis-cat-coi-999        .
           perform   vis-cat-cof-000      thru vis-cat-cof-999        .
           perform   vis-cat-mni-000      thru vis-cat-mni-999        .
           perform   vis-cat-mnf-000      thru vis-cat-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Codice statistico                       *
      *                      *-----------------------------------------*
           perform   vis-stt-dei-000      thru vis-stt-dei-999        .
           perform   vis-stt-def-000      thru vis-stt-def-999        .
           perform   vis-stt-coi-000      thru vis-stt-coi-999        .
           perform   vis-stt-cof-000      thru vis-stt-cof-999        .
           perform   vis-stt-mni-000      thru vis-stt-mni-999        .
           perform   vis-stt-mnf-000      thru vis-stt-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Codice nazione                          *
      *                      *-----------------------------------------*
           perform   vis-naz-dei-000      thru vis-naz-dei-999        .
           perform   vis-naz-def-000      thru vis-naz-def-999        .
           perform   vis-naz-coi-000      thru vis-naz-coi-999        .
           perform   vis-naz-cof-000      thru vis-naz-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice regione                          *
      *                      *-----------------------------------------*
           perform   vis-rgn-dei-000      thru vis-rgn-dei-999        .
           perform   vis-rgn-def-000      thru vis-rgn-def-999        .
           perform   vis-rgn-coi-000      thru vis-rgn-coi-999        .
           perform   vis-rgn-cof-000      thru vis-rgn-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice provincia                        *
      *                      *-----------------------------------------*
           perform   vis-prv-dei-000      thru vis-prv-dei-999        .
           perform   vis-prv-def-000      thru vis-prv-def-999        .
           perform   vis-prv-coi-000      thru vis-prv-coi-999        .
           perform   vis-prv-cof-000      thru vis-prv-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice comune                           *
      *                      *-----------------------------------------*
           perform   vis-cmn-dei-000      thru vis-cmn-dei-999        .
           perform   vis-cmn-def-000      thru vis-cmn-def-999        .
           perform   vis-cmn-cpi-000      thru vis-cmn-cpi-999        .
           perform   vis-cmn-cpi-000      thru vis-cmn-cpi-999        .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-ord-ana-naz-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-ord-ana-naz-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-ord-ana-naz-100.
       acc-ord-ana-naz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo ordinamento per codice       *
      *    *                         nazione                           *
      *    *-----------------------------------------------------------*
       vis-ord-ana-naz-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ana-naz-lun    to   v-car                  .
           move      w-exp-ana-naz-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-lpt-ord-ana-lin    to   v-lin                  .
           move      w-lpt-ord-ana-pos    to   v-pos                  .
           move      w-exp-ana-naz-tbl    to   v-txt                  .
           move      rr-ord-ana-naz       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ord-ana-naz-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo ordinamento per codice regione  *
      *    *-----------------------------------------------------------*
       acc-ord-ana-rgn-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore attuale                  *
      *                  *---------------------------------------------*
           move      rr-ord-ana-rgn       to   w-sav-ana-rgn          .
       acc-ord-ana-rgn-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ana-rgn-lun    to   v-car                  .
           move      w-exp-ana-rgn-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-lpt-ord-ana-lin    to   v-lin                  .
           move      w-lpt-ord-ana-pos    to   v-pos                  .
           move      w-exp-ana-rgn-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-ord-ana-rgn       to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-ord-ana-rgn-999.
       acc-ord-ana-rgn-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-ord-ana-rgn         .
       acc-ord-ana-rgn-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-ord-ana-rgn       =    zero
                     go to acc-ord-ana-rgn-100.
       acc-ord-ana-rgn-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore variato                      *
      *                  *---------------------------------------------*
           if        rr-ord-ana-rgn       =    w-sav-ana-rgn
                     go to acc-ord-ana-rgn-800.
       acc-ord-ana-rgn-620.
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori richieste            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica fornitori       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-fnt-000  thru nor-ric-sel-fnt-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica zone            *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-zon-000  thru nor-ric-sel-zon-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica categorie       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-cat-000  thru nor-ric-sel-cat-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica statistici      *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-stt-000  thru nor-ric-sel-stt-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica nazioni         *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-naz-000  thru nor-ric-sel-naz-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica regioni         *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-rgn-000  thru nor-ric-sel-rgn-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica provincie       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-prv-000  thru nor-ric-sel-prv-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica comuni          *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-cmn-000  thru nor-ric-sel-cmn-999    .
      *                      *-----------------------------------------*
      *                      * Selettori supplementari                 *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-ssu-000  thru nor-ric-sel-ssu-999    .
       acc-ord-ana-rgn-640.
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts richieste           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafiche iniziale       *
      *                      *-----------------------------------------*
           perform   pmt-sel-ain-000      thru pmt-sel-ain-999        .
      *                      *-----------------------------------------*
      *                      * Selettore su anagrafiche finale         *
      *                      *-----------------------------------------*
           perform   pmt-sel-afi-000      thru pmt-sel-afi-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione valori richieste            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Fornitore                               *
      *                      *-----------------------------------------*
           perform   vis-fnt-rsi-000      thru vis-fnt-rsi-999        .
           perform   vis-fnt-rsf-000      thru vis-fnt-rsf-999        .
           perform   vis-fnt-coi-000      thru vis-fnt-coi-999        .
           perform   vis-fnt-cof-000      thru vis-fnt-cof-999        .
           perform   vis-fnt-mni-000      thru vis-fnt-mni-999        .
           perform   vis-fnt-mnf-000      thru vis-fnt-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Zona                                    *
      *                      *-----------------------------------------*
           perform   vis-zon-dei-000      thru vis-zon-dei-999        .
           perform   vis-zon-def-000      thru vis-zon-def-999        .
           perform   vis-zon-coi-000      thru vis-zon-coi-999        .
           perform   vis-zon-cof-000      thru vis-zon-cof-999        .
           perform   vis-zon-mni-000      thru vis-zon-mni-999        .
           perform   vis-zon-mnf-000      thru vis-zon-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Categorie                               *
      *                      *-----------------------------------------*
           perform   vis-cat-dei-000      thru vis-cat-dei-999        .
           perform   vis-cat-def-000      thru vis-cat-def-999        .
           perform   vis-cat-coi-000      thru vis-cat-coi-999        .
           perform   vis-cat-cof-000      thru vis-cat-cof-999        .
           perform   vis-cat-mni-000      thru vis-cat-mni-999        .
           perform   vis-cat-mnf-000      thru vis-cat-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Codice statistico                       *
      *                      *-----------------------------------------*
           perform   vis-stt-dei-000      thru vis-stt-dei-999        .
           perform   vis-stt-def-000      thru vis-stt-def-999        .
           perform   vis-stt-coi-000      thru vis-stt-coi-999        .
           perform   vis-stt-cof-000      thru vis-stt-cof-999        .
           perform   vis-stt-mni-000      thru vis-stt-mni-999        .
           perform   vis-stt-mnf-000      thru vis-stt-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Codice nazione                          *
      *                      *-----------------------------------------*
           perform   vis-naz-dei-000      thru vis-naz-dei-999        .
           perform   vis-naz-def-000      thru vis-naz-def-999        .
           perform   vis-naz-coi-000      thru vis-naz-coi-999        .
           perform   vis-naz-cof-000      thru vis-naz-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice regione                          *
      *                      *-----------------------------------------*
           perform   vis-rgn-dei-000      thru vis-rgn-dei-999        .
           perform   vis-rgn-def-000      thru vis-rgn-def-999        .
           perform   vis-rgn-coi-000      thru vis-rgn-coi-999        .
           perform   vis-rgn-cof-000      thru vis-rgn-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice provincia                        *
      *                      *-----------------------------------------*
           perform   vis-prv-dei-000      thru vis-prv-dei-999        .
           perform   vis-prv-def-000      thru vis-prv-def-999        .
           perform   vis-prv-coi-000      thru vis-prv-coi-999        .
           perform   vis-prv-cof-000      thru vis-prv-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice comune                           *
      *                      *-----------------------------------------*
           perform   vis-cmn-dei-000      thru vis-cmn-dei-999        .
           perform   vis-cmn-def-000      thru vis-cmn-def-999        .
           perform   vis-cmn-cpi-000      thru vis-cmn-cpi-999        .
           perform   vis-cmn-cpi-000      thru vis-cmn-cpi-999        .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-ord-ana-rgn-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-ord-ana-rgn-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-ord-ana-rgn-100.
       acc-ord-ana-rgn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo ordinamento per codice       *
      *    *                         regione                           *
      *    *-----------------------------------------------------------*
       vis-ord-ana-rgn-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ana-rgn-lun    to   v-car                  .
           move      w-exp-ana-rgn-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-lpt-ord-ana-lin    to   v-lin                  .
           move      w-lpt-ord-ana-pos    to   v-pos                  .
           move      w-exp-ana-rgn-tbl    to   v-txt                  .
           move      rr-ord-ana-rgn       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ord-ana-rgn-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo ordinamento per codice provin-  *
      *    *                      cia                                  *
      *    *-----------------------------------------------------------*
       acc-ord-ana-prv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore attuale                  *
      *                  *---------------------------------------------*
           move      rr-ord-ana-prv       to   w-sav-ana-prv          .
       acc-ord-ana-prv-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ana-prv-lun    to   v-car                  .
           move      w-exp-ana-prv-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-lpt-ord-ana-lin    to   v-lin                  .
           move      w-lpt-ord-ana-pos    to   v-pos                  .
           move      w-exp-ana-prv-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-ord-ana-prv       to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-ord-ana-prv-999.
       acc-ord-ana-prv-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-ord-ana-prv         .
       acc-ord-ana-prv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-ord-ana-prv       =    zero
                     go to acc-ord-ana-prv-100.
       acc-ord-ana-prv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore variato                      *
      *                  *---------------------------------------------*
           if        rr-ord-ana-prv       =    w-sav-ana-prv
                     go to acc-ord-ana-prv-800.
       acc-ord-ana-prv-620.
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori richieste            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica fornitori       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-fnt-000  thru nor-ric-sel-fnt-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica zone            *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-zon-000  thru nor-ric-sel-zon-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica categorie       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-cat-000  thru nor-ric-sel-cat-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica statistici      *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-stt-000  thru nor-ric-sel-stt-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica nazioni         *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-naz-000  thru nor-ric-sel-naz-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica regioni         *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-rgn-000  thru nor-ric-sel-rgn-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica provincie       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-prv-000  thru nor-ric-sel-prv-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica comuni          *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-cmn-000  thru nor-ric-sel-cmn-999    .
      *                      *-----------------------------------------*
      *                      * Selettori supplementari                 *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-ssu-000  thru nor-ric-sel-ssu-999    .
       acc-ord-ana-prv-640.
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts richieste           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafiche iniziale       *
      *                      *-----------------------------------------*
           perform   pmt-sel-ain-000      thru pmt-sel-ain-999        .
      *                      *-----------------------------------------*
      *                      * Selettore su anagrafiche finale         *
      *                      *-----------------------------------------*
           perform   pmt-sel-afi-000      thru pmt-sel-afi-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione valori richieste            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Fornitore                               *
      *                      *-----------------------------------------*
           perform   vis-fnt-rsi-000      thru vis-fnt-rsi-999        .
           perform   vis-fnt-rsf-000      thru vis-fnt-rsf-999        .
           perform   vis-fnt-coi-000      thru vis-fnt-coi-999        .
           perform   vis-fnt-cof-000      thru vis-fnt-cof-999        .
           perform   vis-fnt-mni-000      thru vis-fnt-mni-999        .
           perform   vis-fnt-mnf-000      thru vis-fnt-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Zona                                    *
      *                      *-----------------------------------------*
           perform   vis-zon-dei-000      thru vis-zon-dei-999        .
           perform   vis-zon-def-000      thru vis-zon-def-999        .
           perform   vis-zon-coi-000      thru vis-zon-coi-999        .
           perform   vis-zon-cof-000      thru vis-zon-cof-999        .
           perform   vis-zon-mni-000      thru vis-zon-mni-999        .
           perform   vis-zon-mnf-000      thru vis-zon-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Categorie                               *
      *                      *-----------------------------------------*
           perform   vis-cat-dei-000      thru vis-cat-dei-999        .
           perform   vis-cat-def-000      thru vis-cat-def-999        .
           perform   vis-cat-coi-000      thru vis-cat-coi-999        .
           perform   vis-cat-cof-000      thru vis-cat-cof-999        .
           perform   vis-cat-mni-000      thru vis-cat-mni-999        .
           perform   vis-cat-mnf-000      thru vis-cat-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Codice statistico                       *
      *                      *-----------------------------------------*
           perform   vis-stt-dei-000      thru vis-stt-dei-999        .
           perform   vis-stt-def-000      thru vis-stt-def-999        .
           perform   vis-stt-coi-000      thru vis-stt-coi-999        .
           perform   vis-stt-cof-000      thru vis-stt-cof-999        .
           perform   vis-stt-mni-000      thru vis-stt-mni-999        .
           perform   vis-stt-mnf-000      thru vis-stt-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Codice nazione                          *
      *                      *-----------------------------------------*
           perform   vis-naz-dei-000      thru vis-naz-dei-999        .
           perform   vis-naz-def-000      thru vis-naz-def-999        .
           perform   vis-naz-coi-000      thru vis-naz-coi-999        .
           perform   vis-naz-cof-000      thru vis-naz-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice regione                          *
      *                      *-----------------------------------------*
           perform   vis-rgn-dei-000      thru vis-rgn-dei-999        .
           perform   vis-rgn-def-000      thru vis-rgn-def-999        .
           perform   vis-rgn-coi-000      thru vis-rgn-coi-999        .
           perform   vis-rgn-cof-000      thru vis-rgn-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice provincia                        *
      *                      *-----------------------------------------*
           perform   vis-prv-dei-000      thru vis-prv-dei-999        .
           perform   vis-prv-def-000      thru vis-prv-def-999        .
           perform   vis-prv-coi-000      thru vis-prv-coi-999        .
           perform   vis-prv-cof-000      thru vis-prv-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice comune                           *
      *                      *-----------------------------------------*
           perform   vis-cmn-dei-000      thru vis-cmn-dei-999        .
           perform   vis-cmn-def-000      thru vis-cmn-def-999        .
           perform   vis-cmn-cpi-000      thru vis-cmn-cpi-999        .
           perform   vis-cmn-cpi-000      thru vis-cmn-cpi-999        .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-ord-ana-prv-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-ord-ana-prv-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-ord-ana-prv-100.
       acc-ord-ana-prv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo ordinamento per codice       *
      *    *                         provincia                         *
      *    *-----------------------------------------------------------*
       vis-ord-ana-prv-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ana-prv-lun    to   v-car                  .
           move      w-exp-ana-prv-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-lpt-ord-ana-lin    to   v-lin                  .
           move      w-lpt-ord-ana-pos    to   v-pos                  .
           move      w-exp-ana-prv-tbl    to   v-txt                  .
           move      rr-ord-ana-prv       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ord-ana-prv-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo ordinamento per codice comune   *
      *    *-----------------------------------------------------------*
       acc-ord-ana-cmn-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore attuale                  *
      *                  *---------------------------------------------*
           move      rr-ord-ana-cmn       to   w-sav-ana-cmn          .
       acc-ord-ana-cmn-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ana-cmn-lun    to   v-car                  .
           move      w-exp-ana-cmn-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-lpt-ord-ana-lin    to   v-lin                  .
           move      w-lpt-ord-ana-pos    to   v-pos                  .
           move      w-exp-ana-cmn-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-ord-ana-cmn       to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-ord-ana-cmn-999.
       acc-ord-ana-cmn-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-ord-ana-cmn         .
       acc-ord-ana-cmn-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-ord-ana-cmn       =    zero
                     go to acc-ord-ana-cmn-100.
       acc-ord-ana-cmn-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore variato                      *
      *                  *---------------------------------------------*
           if        rr-ord-ana-cmn       =    w-sav-ana-cmn
                     go to acc-ord-ana-cmn-800.
       acc-ord-ana-cmn-620.
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori richieste            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica fornitori       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-fnt-000  thru nor-ric-sel-fnt-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica zone            *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-zon-000  thru nor-ric-sel-zon-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica categorie       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-cat-000  thru nor-ric-sel-cat-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica statistici      *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-stt-000  thru nor-ric-sel-stt-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica nazioni         *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-naz-000  thru nor-ric-sel-naz-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica regioni         *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-rgn-000  thru nor-ric-sel-rgn-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica provincie       *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-prv-000  thru nor-ric-sel-prv-999    .
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafica comuni          *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-cmn-000  thru nor-ric-sel-cmn-999    .
      *                      *-----------------------------------------*
      *                      * Selettori supplementari                 *
      *                      *-----------------------------------------*
           perform   nor-ric-sel-ssu-000  thru nor-ric-sel-ssu-999    .
       acc-ord-ana-cmn-640.
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts richieste           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Selettori su anagrafiche iniziale       *
      *                      *-----------------------------------------*
           perform   pmt-sel-ain-000      thru pmt-sel-ain-999        .
      *                      *-----------------------------------------*
      *                      * Selettore su anagrafiche finale         *
      *                      *-----------------------------------------*
           perform   pmt-sel-afi-000      thru pmt-sel-afi-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione valori richieste            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Fornitore                               *
      *                      *-----------------------------------------*
           perform   vis-fnt-rsi-000      thru vis-fnt-rsi-999        .
           perform   vis-fnt-rsf-000      thru vis-fnt-rsf-999        .
           perform   vis-fnt-coi-000      thru vis-fnt-coi-999        .
           perform   vis-fnt-cof-000      thru vis-fnt-cof-999        .
           perform   vis-fnt-mni-000      thru vis-fnt-mni-999        .
           perform   vis-fnt-mnf-000      thru vis-fnt-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Zona                                    *
      *                      *-----------------------------------------*
           perform   vis-zon-dei-000      thru vis-zon-dei-999        .
           perform   vis-zon-def-000      thru vis-zon-def-999        .
           perform   vis-zon-coi-000      thru vis-zon-coi-999        .
           perform   vis-zon-cof-000      thru vis-zon-cof-999        .
           perform   vis-zon-mni-000      thru vis-zon-mni-999        .
           perform   vis-zon-mnf-000      thru vis-zon-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Categorie                               *
      *                      *-----------------------------------------*
           perform   vis-cat-dei-000      thru vis-cat-dei-999        .
           perform   vis-cat-def-000      thru vis-cat-def-999        .
           perform   vis-cat-coi-000      thru vis-cat-coi-999        .
           perform   vis-cat-cof-000      thru vis-cat-cof-999        .
           perform   vis-cat-mni-000      thru vis-cat-mni-999        .
           perform   vis-cat-mnf-000      thru vis-cat-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Codice statistico                       *
      *                      *-----------------------------------------*
           perform   vis-stt-dei-000      thru vis-stt-dei-999        .
           perform   vis-stt-def-000      thru vis-stt-def-999        .
           perform   vis-stt-coi-000      thru vis-stt-coi-999        .
           perform   vis-stt-cof-000      thru vis-stt-cof-999        .
           perform   vis-stt-mni-000      thru vis-stt-mni-999        .
           perform   vis-stt-mnf-000      thru vis-stt-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Codice nazione                          *
      *                      *-----------------------------------------*
           perform   vis-naz-dei-000      thru vis-naz-dei-999        .
           perform   vis-naz-def-000      thru vis-naz-def-999        .
           perform   vis-naz-coi-000      thru vis-naz-coi-999        .
           perform   vis-naz-cof-000      thru vis-naz-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice regione                          *
      *                      *-----------------------------------------*
           perform   vis-rgn-dei-000      thru vis-rgn-dei-999        .
           perform   vis-rgn-def-000      thru vis-rgn-def-999        .
           perform   vis-rgn-coi-000      thru vis-rgn-coi-999        .
           perform   vis-rgn-cof-000      thru vis-rgn-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice provincia                        *
      *                      *-----------------------------------------*
           perform   vis-prv-dei-000      thru vis-prv-dei-999        .
           perform   vis-prv-def-000      thru vis-prv-def-999        .
           perform   vis-prv-coi-000      thru vis-prv-coi-999        .
           perform   vis-prv-cof-000      thru vis-prv-cof-999        .
      *                      *-----------------------------------------*
      *                      * Codice comune                           *
      *                      *-----------------------------------------*
           perform   vis-cmn-dei-000      thru vis-cmn-dei-999        .
           perform   vis-cmn-def-000      thru vis-cmn-def-999        .
           perform   vis-cmn-cpi-000      thru vis-cmn-cpi-999        .
           perform   vis-cmn-cpi-000      thru vis-cmn-cpi-999        .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-ord-ana-cmn-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-ord-ana-cmn-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-ord-ana-cmn-100.
       acc-ord-ana-cmn-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo ordinamento per codice       *
      *    *                         comune                            *
      *    *-----------------------------------------------------------*
       vis-ord-ana-cmn-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ana-cmn-lun    to   v-car                  .
           move      w-exp-ana-cmn-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-lpt-ord-ana-lin    to   v-lin                  .
           move      w-lpt-ord-ana-pos    to   v-pos                  .
           move      w-exp-ana-cmn-tbl    to   v-txt                  .
           move      rr-ord-ana-cmn       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ord-ana-cmn-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Selettori su anagrafiche iniziale    *
      *    *-----------------------------------------------------------*
       acc-sel-ain-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del Tipo ordinamento     *
      *              * stampa                                          *
      *              *-------------------------------------------------*
           if        rr-tip-ors           =    01
                     go to acc-sel-ain-100
           else if   rr-tip-ors           =    03
                     go to acc-sel-ain-300
           else if   rr-tip-ors           =    04
                     go to acc-sel-ain-400
           else if   rr-tip-ors           =    05
                     go to acc-sel-ain-500
           else if   rr-tip-ors           =    07
                     go to acc-sel-ain-a00
           else if   rr-tip-ors           =    11
                     go to acc-sel-ain-610
           else if   rr-tip-ors           =    12
                     go to acc-sel-ain-620
           else if   rr-tip-ors           =    13
                     go to acc-sel-ain-630
           else if   rr-tip-ors           =    14
                     go to acc-sel-ain-640
           else if   rr-tip-ors           =    21 or
                     rr-tip-ors           =    22
                     go to acc-sel-ain-710
           else      go to acc-sel-ain-800.
       acc-sel-ain-100.
      *              *-------------------------------------------------*
      *              * Se ordinamento per fornitore                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica fornitore                        *
      *                  *---------------------------------------------*
           if        rr-ord-ana-fnt       =    01
                     go to acc-sel-ain-110
           else if   rr-ord-ana-fnt       =    02
                     go to acc-sel-ain-120
           else if   rr-ord-ana-fnt       =    03
                     go to acc-sel-ain-130
           else      go to acc-sel-ain-190.
       acc-sel-ain-110.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per ragione sociale      *
      *                      *-----------------------------------------*
           perform   acc-fnt-rsi-000      thru acc-fnt-rsi-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-120.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           perform   acc-fnt-coi-000      thru acc-fnt-coi-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-130.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per mnemonico            *
      *                      *-----------------------------------------*
           perform   acc-fnt-mni-000      thru acc-fnt-mni-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-190.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-300.
      *              *-------------------------------------------------*
      *              * Se ordinamento per zona                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica zona                             *
      *                  *---------------------------------------------*
           if        rr-ord-ana-zon       =    01
                     go to acc-sel-ain-310
           else if   rr-ord-ana-zon       =    02
                     go to acc-sel-ain-320
           else if   rr-ord-ana-zon       =    03
                     go to acc-sel-ain-330
           else      go to acc-sel-ain-390.
       acc-sel-ain-310.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           perform   acc-zon-coi-000      thru acc-zon-coi-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-320.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           perform   acc-zon-dei-000      thru acc-zon-dei-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-330.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per mnemonico            *
      *                      *-----------------------------------------*
           perform   acc-zon-mni-000      thru acc-zon-mni-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-390.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-400.
      *              *-------------------------------------------------*
      *              * Se ordinamento per categorie                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica categoria                        *
      *                  *---------------------------------------------*
           if        rr-ord-ana-cat       =    01
                     go to acc-sel-ain-410
           else if   rr-ord-ana-cat       =    02
                     go to acc-sel-ain-420
           else if   rr-ord-ana-cat       =    03
                     go to acc-sel-ain-430
           else      go to acc-sel-ain-490.
       acc-sel-ain-410.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           perform   acc-cat-coi-000      thru acc-cat-coi-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-420.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           perform   acc-cat-dei-000      thru acc-cat-dei-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-430.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per mnemonico            *
      *                      *-----------------------------------------*
           perform   acc-cat-mni-000      thru acc-cat-mni-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-490.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-500.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici statistici            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica codice statistico                *
      *                  *---------------------------------------------*
           if        rr-ord-ana-stt       =    01
                     go to acc-sel-ain-510
           else if   rr-ord-ana-stt       =    02
                     go to acc-sel-ain-520
           else if   rr-ord-ana-stt       =    03
                     go to acc-sel-ain-530
           else      go to acc-sel-ain-590.
       acc-sel-ain-510.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           perform   acc-stt-coi-000      thru acc-stt-coi-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-520.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           perform   acc-stt-dei-000      thru acc-stt-dei-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-530.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per mnemonico            *
      *                      *-----------------------------------------*
           perform   acc-stt-mni-000      thru acc-stt-mni-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-590.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-a00.
      *              *-------------------------------------------------*
      *              * Se ordinamento per forme di pagamento           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica forme di pagamento               *
      *                  *---------------------------------------------*
           if        rr-ord-ana-fop       =    01
                     go to acc-sel-ain-a02
           else if   rr-ord-ana-fop       =    02
                     go to acc-sel-ain-a04
           else if   rr-ord-ana-fop       =    03
                     go to acc-sel-ain-a06
           else      go to acc-sel-ain-a09.
       acc-sel-ain-a02.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           perform   acc-fop-coi-000      thru acc-fop-coi-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-a04.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           perform   acc-fop-dei-000      thru acc-fop-dei-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-a06.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per mnemonico            *
      *                      *-----------------------------------------*
           perform   acc-fop-mni-000      thru acc-fop-mni-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-a09.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-610.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici nazione               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica codice nazione                   *
      *                  *---------------------------------------------*
           if        rr-ord-ana-naz       =    01
                     go to acc-sel-ain-611
           else if   rr-ord-ana-naz       =    02
                     go to acc-sel-ain-612
           else      go to acc-sel-ain-615.
       acc-sel-ain-611.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           perform   acc-naz-coi-000      thru acc-naz-coi-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-612.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           perform   acc-naz-dei-000      thru acc-naz-dei-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-615.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-620.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici regione               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica codice regione                   *
      *                  *---------------------------------------------*
           if        rr-ord-ana-rgn       =    01
                     go to acc-sel-ain-621
           else if   rr-ord-ana-rgn       =    02
                     go to acc-sel-ain-622
           else      go to acc-sel-ain-625.
       acc-sel-ain-621.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           perform   acc-rgn-coi-000      thru acc-rgn-coi-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-622.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           perform   acc-rgn-dei-000      thru acc-rgn-dei-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-625.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-630.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici provincia             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica codice provincia                 *
      *                  *---------------------------------------------*
           if        rr-ord-ana-prv       =    01
                     go to acc-sel-ain-631
           else if   rr-ord-ana-prv       =    02
                     go to acc-sel-ain-632
           else      go to acc-sel-ain-635.
       acc-sel-ain-631.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           perform   acc-prv-coi-000      thru acc-prv-coi-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-632.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           perform   acc-prv-dei-000      thru acc-prv-dei-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-635.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-640.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici comune                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica codice comune                    *
      *                  *---------------------------------------------*
           if        rr-ord-ana-cmn       =    01
                     go to acc-sel-ain-641
           else if   rr-ord-ana-cmn       =    02
                     go to acc-sel-ain-642
           else      go to acc-sel-ain-645.
       acc-sel-ain-641.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           perform   acc-cmn-dei-000      thru acc-cmn-dei-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-642.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per C.a.p.               *
      *                      *-----------------------------------------*
           perform   acc-cmn-cpi-000      thru acc-cmn-cpi-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-645.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-710.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici di avviamento postale *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita senza alcuna azione               *
      *                  *---------------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-800.
      *              *-------------------------------------------------*
      *              * Se ordinamento non definito                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     acc-sel-ain-900.
       acc-sel-ain-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-sel-ain-999.
       acc-sel-ain-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Selettori su anagrafiche finale      *
      *    *-----------------------------------------------------------*
       acc-sel-afi-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del Tipo ordinamento     *
      *              * stampa                                          *
      *              *-------------------------------------------------*
           if        rr-tip-ors           =    01
                     go to acc-sel-afi-100
           else if   rr-tip-ors           =    03
                     go to acc-sel-afi-300
           else if   rr-tip-ors           =    04
                     go to acc-sel-afi-400
           else if   rr-tip-ors           =    05
                     go to acc-sel-afi-500
           else if   rr-tip-ors           =    07
                     go to acc-sel-afi-a00
           else if   rr-tip-ors           =    11
                     go to acc-sel-afi-610
           else if   rr-tip-ors           =    12
                     go to acc-sel-afi-620
           else if   rr-tip-ors           =    13
                     go to acc-sel-afi-630
           else if   rr-tip-ors           =    14
                     go to acc-sel-afi-640
           else if   rr-tip-ors           =    21 or
                     rr-tip-ors           =    22
                     go to acc-sel-afi-710
           else      go to acc-sel-afi-800.
       acc-sel-afi-100.
      *              *-------------------------------------------------*
      *              * Se ordinamento per fornitore                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica fornitore                        *
      *                  *---------------------------------------------*
           if        rr-ord-ana-fnt       =    01
                     go to acc-sel-afi-110
           else if   rr-ord-ana-fnt       =    02
                     go to acc-sel-afi-120
           else if   rr-ord-ana-fnt       =    03
                     go to acc-sel-afi-130
           else      go to acc-sel-afi-190.
       acc-sel-afi-110.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per ragione sociale      *
      *                      *-----------------------------------------*
           perform   acc-fnt-rsf-000      thru acc-fnt-rsf-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-120.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           perform   acc-fnt-cof-000      thru acc-fnt-cof-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-130.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per mnemonico            *
      *                      *-----------------------------------------*
           perform   acc-fnt-mnf-000      thru acc-fnt-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-190.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-300.
      *              *-------------------------------------------------*
      *              * Se ordinamento per zona                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica zona                             *
      *                  *---------------------------------------------*
           if        rr-ord-ana-zon       =    01
                     go to acc-sel-afi-310
           else if   rr-ord-ana-zon       =    02
                     go to acc-sel-afi-320
           else if   rr-ord-ana-zon       =    03
                     go to acc-sel-afi-330
           else      go to acc-sel-afi-390.
       acc-sel-afi-310.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           perform   acc-zon-cof-000      thru acc-zon-cof-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-320.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           perform   acc-zon-def-000      thru acc-zon-def-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-330.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per mnemonico            *
      *                      *-----------------------------------------*
           perform   acc-zon-mnf-000      thru acc-zon-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-390.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-400.
      *              *-------------------------------------------------*
      *              * Se ordinamento per categorie                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica categoria                        *
      *                  *---------------------------------------------*
           if        rr-ord-ana-cat       =    01
                     go to acc-sel-afi-410
           else if   rr-ord-ana-cat       =    02
                     go to acc-sel-afi-420
           else if   rr-ord-ana-cat       =    03
                     go to acc-sel-afi-430
           else      go to acc-sel-afi-490.
       acc-sel-afi-410.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           perform   acc-cat-cof-000      thru acc-cat-cof-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-420.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           perform   acc-cat-def-000      thru acc-cat-def-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-430.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per mnemonico            *
      *                      *-----------------------------------------*
           perform   acc-cat-mnf-000      thru acc-cat-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-490.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-500.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici statistici            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica codice statistico                *
      *                  *---------------------------------------------*
           if        rr-ord-ana-stt       =    01
                     go to acc-sel-afi-510
           else if   rr-ord-ana-stt       =    02
                     go to acc-sel-afi-520
           else if   rr-ord-ana-stt       =    03
                     go to acc-sel-afi-530
           else      go to acc-sel-afi-590.
       acc-sel-afi-510.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           perform   acc-stt-cof-000      thru acc-stt-cof-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-520.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           perform   acc-stt-def-000      thru acc-stt-def-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-530.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per mnemonico            *
      *                      *-----------------------------------------*
           perform   acc-stt-mnf-000      thru acc-stt-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-590.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-610.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici nazione               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica codice nazione                   *
      *                  *---------------------------------------------*
           if        rr-ord-ana-naz       =    01
                     go to acc-sel-afi-611
           else if   rr-ord-ana-naz       =    02
                     go to acc-sel-afi-612
           else      go to acc-sel-afi-615.
       acc-sel-afi-611.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           perform   acc-naz-cof-000      thru acc-naz-cof-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-612.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           perform   acc-naz-def-000      thru acc-naz-def-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-615.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-620.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici regione               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica codice regione                   *
      *                  *---------------------------------------------*
           if        rr-ord-ana-rgn       =    01
                     go to acc-sel-afi-621
           else if   rr-ord-ana-rgn       =    02
                     go to acc-sel-afi-622
           else      go to acc-sel-afi-625.
       acc-sel-afi-621.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           perform   acc-rgn-cof-000      thru acc-rgn-cof-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-622.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           perform   acc-rgn-def-000      thru acc-rgn-def-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-625.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-630.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici provincia             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica codice provincia                 *
      *                  *---------------------------------------------*
           if        rr-ord-ana-prv       =    01
                     go to acc-sel-afi-631
           else if   rr-ord-ana-prv       =    02
                     go to acc-sel-afi-632
           else      go to acc-sel-afi-635.
       acc-sel-afi-631.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           perform   acc-prv-cof-000      thru acc-prv-cof-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-632.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           perform   acc-prv-def-000      thru acc-prv-def-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-635.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-640.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici comune                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica codice comune                    *
      *                  *---------------------------------------------*
           if        rr-ord-ana-cmn       =    01
                     go to acc-sel-afi-641
           else if   rr-ord-ana-cmn       =    02
                     go to acc-sel-afi-642
           else      go to acc-sel-afi-645.
       acc-sel-afi-641.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           perform   acc-cmn-def-000      thru acc-cmn-def-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-642.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per C.a.p.               *
      *                      *-----------------------------------------*
           perform   acc-cmn-cpf-000      thru acc-cmn-cpf-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-645.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-710.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici di avviamento postale *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita senza alcuna azione               *
      *                  *---------------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-a00.
      *              *-------------------------------------------------*
      *              * Se ordinamento per forme di pagamento           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  * anagrafica forme di pagamento               *
      *                  *---------------------------------------------*
           if        rr-ord-ana-fop       =    01
                     go to acc-sel-afi-a02
           else if   rr-ord-ana-fop       =    02
                     go to acc-sel-afi-a04
           else if   rr-ord-ana-fop       =    03
                     go to acc-sel-afi-a06
           else      go to acc-sel-afi-a09.
       acc-sel-afi-a02.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per codice               *
      *                      *-----------------------------------------*
           perform   acc-fop-cof-000      thru acc-fop-cof-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-a04.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per descrizione          *
      *                      *-----------------------------------------*
           perform   acc-fop-def-000      thru acc-fop-def-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-a06.
      *                      *-----------------------------------------*
      *                      * Se ordinamento per mnemonico            *
      *                      *-----------------------------------------*
           perform   acc-fop-mnf-000      thru acc-fop-mnf-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-a09.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-800.
      *              *-------------------------------------------------*
      *              * Se ordinamento non definito                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     acc-sel-afi-900.
       acc-sel-afi-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-sel-afi-999.
       acc-sel-afi-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Fornitore - ragione sociale iniziale *
      *    *-----------------------------------------------------------*
       acc-fnt-rsi-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-fnt-rsi-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-fnt-rsi       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-fnt-rsi-999.
       acc-fnt-rsi-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-fnt-rsi         .
       acc-fnt-rsi-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-fnt-rsi-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-fnt-rsi-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-fnt-rsi-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-fnt-rsi-100.
       acc-fnt-rsi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Fornitore - ragione sociale ini-  *
      *    *                         ziale                             *
      *    *-----------------------------------------------------------*
       vis-fnt-rsi-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      rr-sel-fnt-rsi       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-fnt-rsi-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Fornitore - ragione sociale finale   *
      *    *-----------------------------------------------------------*
       acc-fnt-rsf-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-fnt-rsf-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-fnt-rsf       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-fnt-rsf-999.
       acc-fnt-rsf-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-fnt-rsf         .
       acc-fnt-rsf-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-fnt-rsf-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-fnt-rsf-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-fnt-rsf-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-fnt-rsf-100.
       acc-fnt-rsf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Fornitore - ragione sociale fina- *
      *    *                         le                                *
      *    *-----------------------------------------------------------*
       vis-fnt-rsf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      rr-sel-fnt-rsf       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-fnt-rsf-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Fornitore - codice iniziale          *
      *    *-----------------------------------------------------------*
       acc-fnt-coi-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-fnt-coi-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-dcf-ope      .
           move      rr-sel-fnt-coi       to   w-cod-mne-dcf-cod      .
           move      w-lpt-sel-ain-lin    to   w-cod-mne-dcf-lin      .
           move      w-lpt-sel-ain-pos    to   w-cod-mne-dcf-pos      .
           move      zero                 to   w-cod-mne-dcf-rln      .
           move      zero                 to   w-cod-mne-dcf-rps      .
           move      zero                 to   w-cod-mne-dcf-vln      .
           move      zero                 to   w-cod-mne-dcf-vps      .
           move      zero                 to   w-cod-mne-dcf-lln      .
           move      zero                 to   w-cod-mne-dcf-lps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-dcf-cll-000  thru cod-mne-dcf-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-dcf-foi-000  thru cod-mne-dcf-foi-999    .
       acc-fnt-coi-110.
           perform   cod-mne-dcf-cll-000  thru cod-mne-dcf-cll-999    .
           if        w-cod-mne-dcf-ope    =    "F+"
                     go to acc-fnt-coi-115.
           if        w-cod-mne-dcf-ope    =    "AC"
                     go to acc-fnt-coi-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-fnt-coi-115.
           perform   cod-mne-dcf-foi-000  thru cod-mne-dcf-foi-999    .
           go to     acc-fnt-coi-110.
       acc-fnt-coi-120.
           move      w-cod-mne-dcf-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-fnt-coi-999.
       acc-fnt-coi-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-sel-fnt-coi         .
       acc-fnt-coi-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-fnt-coi-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-fnt-coi-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-fnt-coi-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-fnt-coi-100.
       acc-fnt-coi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Fornitore - codice iniziale       *
      *    *-----------------------------------------------------------*
       vis-fnt-coi-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      rr-sel-fnt-coi       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-fnt-coi-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Fornitore - codice finale            *
      *    *-----------------------------------------------------------*
       acc-fnt-cof-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-fnt-cof-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-dcf-ope      .
           move      rr-sel-fnt-cof       to   w-cod-mne-dcf-cod      .
           move      w-lpt-sel-afi-lin    to   w-cod-mne-dcf-lin      .
           move      w-lpt-sel-afi-pos    to   w-cod-mne-dcf-pos      .
           move      zero                 to   w-cod-mne-dcf-rln      .
           move      zero                 to   w-cod-mne-dcf-rps      .
           move      zero                 to   w-cod-mne-dcf-vln      .
           move      zero                 to   w-cod-mne-dcf-vps      .
           move      zero                 to   w-cod-mne-dcf-lln      .
           move      zero                 to   w-cod-mne-dcf-lps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-dcf-cll-000  thru cod-mne-dcf-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-dcf-foi-000  thru cod-mne-dcf-foi-999    .
       acc-fnt-cof-110.
           perform   cod-mne-dcf-cll-000  thru cod-mne-dcf-cll-999    .
           if        w-cod-mne-dcf-ope    =    "F+"
                     go to acc-fnt-cof-115.
           if        w-cod-mne-dcf-ope    =    "AC"
                     go to acc-fnt-cof-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-fnt-cof-115.
           perform   cod-mne-dcf-foi-000  thru cod-mne-dcf-foi-999    .
           go to     acc-fnt-cof-110.
       acc-fnt-cof-120.
           move      w-cod-mne-dcf-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-fnt-cof-999.
       acc-fnt-cof-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-sel-fnt-cof         .
       acc-fnt-cof-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-fnt-cof-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-fnt-cof-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-fnt-cof-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-fnt-cof-100.
       acc-fnt-cof-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Fornitore - codice finale         *
      *    *-----------------------------------------------------------*
       vis-fnt-cof-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      rr-sel-fnt-cof       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-fnt-cof-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Fornitore - mnemonico iniziale       *
      *    *-----------------------------------------------------------*
       acc-fnt-mni-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-fnt-mni-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-fnt-mni       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-fnt-mni-999.
       acc-fnt-mni-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-fnt-mni         .
       acc-fnt-mni-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-fnt-mni-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-fnt-mni-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-fnt-mni-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-fnt-mni-100.
       acc-fnt-mni-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Fornitore - mnemonico iniziale    *
      *    *-----------------------------------------------------------*
       vis-fnt-mni-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      rr-sel-fnt-mni       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-fnt-mni-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Fornitore - mnemonico finale         *
      *    *-----------------------------------------------------------*
       acc-fnt-mnf-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-fnt-mnf-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-fnt-mnf       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-fnt-mnf-999.
       acc-fnt-mnf-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-fnt-mnf         .
       acc-fnt-mnf-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-fnt-mnf-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-fnt-mnf-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-fnt-mnf-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-fnt-mnf-100.
       acc-fnt-mnf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Fornitore - mnemonico finale      *
      *    *-----------------------------------------------------------*
       vis-fnt-mnf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      rr-sel-fnt-mnf       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-fnt-mnf-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Zona - descrizione iniziale          *
      *    *-----------------------------------------------------------*
       acc-zon-dei-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-zon-dei-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-zon-dei       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-zon-dei-999.
       acc-zon-dei-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-zon-dei         .
       acc-zon-dei-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-zon-dei-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-zon-dei-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-zon-dei-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-zon-dei-100.
       acc-zon-dei-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Zona - descrizione iniziale       *
      *    *-----------------------------------------------------------*
       vis-zon-dei-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      rr-sel-zon-dei       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-zon-dei-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Zona - descrizione finale            *
      *    *-----------------------------------------------------------*
       acc-zon-def-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-zon-def-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-zon-def       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-zon-def-999.
       acc-zon-def-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-zon-def         .
       acc-zon-def-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-zon-def-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-zon-def-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-zon-def-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-zon-def-100.
       acc-zon-def-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Zona - descrizione finale         *
      *    *-----------------------------------------------------------*
       vis-zon-def-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      rr-sel-zon-def       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-zon-def-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Zona - codice iniziale               *
      *    *-----------------------------------------------------------*
       acc-zon-coi-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-zon-coi-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-yst-001-ope      .
           move      rr-sel-zon-coi       to   w-cmn-yst-001-cod      .
           move      w-lpt-sel-ain-lin    to   w-cmn-yst-001-lin      .
           move      w-lpt-sel-ain-pos    to   w-cmn-yst-001-pos      .
           move      zero                 to   w-cmn-yst-001-dln      .
           move      zero                 to   w-cmn-yst-001-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cmn-yst-001-cll-000  thru cmn-yst-001-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-yst-001-foi-000  thru cmn-yst-001-foi-999    .
       acc-zon-coi-110.
           perform   cmn-yst-001-cll-000  thru cmn-yst-001-cll-999    .
           if        w-cmn-yst-001-ope    =    "F+"
                     go to acc-zon-coi-115.
           if        w-cmn-yst-001-ope    =    "AC"
                     go to acc-zon-coi-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-zon-coi-115.
           perform   cmn-yst-001-foi-000  thru cmn-yst-001-foi-999    .
           go to     acc-zon-coi-110.
       acc-zon-coi-120.
           move      w-cmn-yst-001-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-zon-coi-999.
       acc-zon-coi-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-sel-zon-coi         .
       acc-zon-coi-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-zon-coi-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-zon-coi-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-zon-coi-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-zon-coi-100.
       acc-zon-coi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Zona - codice iniziale            *
      *    *-----------------------------------------------------------*
       vis-zon-coi-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      rr-sel-zon-coi       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-zon-coi-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Zona - codice finale                 *
      *    *-----------------------------------------------------------*
       acc-zon-cof-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-zon-cof-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-yst-001-ope      .
           move      rr-sel-zon-cof       to   w-cmn-yst-001-cod      .
           move      w-lpt-sel-afi-lin    to   w-cmn-yst-001-lin      .
           move      w-lpt-sel-afi-pos    to   w-cmn-yst-001-pos      .
           move      zero                 to   w-cmn-yst-001-dln      .
           move      zero                 to   w-cmn-yst-001-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cmn-yst-001-cll-000  thru cmn-yst-001-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-yst-001-foi-000  thru cmn-yst-001-foi-999    .
       acc-zon-cof-110.
           perform   cmn-yst-001-cll-000  thru cmn-yst-001-cll-999    .
           if        w-cmn-yst-001-ope    =    "F+"
                     go to acc-zon-cof-115.
           if        w-cmn-yst-001-ope    =    "AC"
                     go to acc-zon-cof-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-zon-cof-115.
           perform   cmn-yst-001-foi-000  thru cmn-yst-001-foi-999    .
           go to     acc-zon-cof-110.
       acc-zon-cof-120.
           move      w-cmn-yst-001-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-zon-cof-999.
       acc-zon-cof-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-sel-zon-cof         .
       acc-zon-cof-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-zon-cof-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-zon-cof-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-zon-cof-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-zon-cof-100.
       acc-zon-cof-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Zona - codice finale              *
      *    *-----------------------------------------------------------*
       vis-zon-cof-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      rr-sel-zon-cof       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-zon-cof-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Zona - mnemonico iniziale            *
      *    *-----------------------------------------------------------*
       acc-zon-mni-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-zon-mni-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-zon-mni       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-zon-mni-999.
       acc-zon-mni-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-zon-mni         .
       acc-zon-mni-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-zon-mni-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-zon-mni-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-zon-mni-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-zon-mni-100.
       acc-zon-mni-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Zona - mnemonico iniziale         *
      *    *-----------------------------------------------------------*
       vis-zon-mni-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      rr-sel-zon-mni       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-zon-mni-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Zona - mnemonico finale              *
      *    *-----------------------------------------------------------*
       acc-zon-mnf-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-zon-mnf-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-zon-mnf       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-zon-mnf-999.
       acc-zon-mnf-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-zon-mnf         .
       acc-zon-mnf-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-zon-mnf-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-zon-mnf-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-zon-mnf-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-zon-mnf-100.
       acc-zon-mnf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Zona - mnemonico finale           *
      *    *-----------------------------------------------------------*
       vis-zon-mnf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      rr-sel-zon-mnf       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-zon-mnf-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Categoria - descrizione iniziale     *
      *    *-----------------------------------------------------------*
       acc-cat-dei-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cat-dei-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-cat-dei       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cat-dei-999.
       acc-cat-dei-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-cat-dei         .
       acc-cat-dei-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cat-dei-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cat-dei-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cat-dei-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cat-dei-100.
       acc-cat-dei-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Categoria - descrizione iniziale  *
      *    *-----------------------------------------------------------*
       vis-cat-dei-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      rr-sel-cat-dei       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cat-dei-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Categoria - descrizione finale       *
      *    *-----------------------------------------------------------*
       acc-cat-def-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cat-def-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-cat-def       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cat-def-999.
       acc-cat-def-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-cat-def         .
       acc-cat-def-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cat-def-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cat-def-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cat-def-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cat-def-100.
       acc-cat-def-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Categoria - descrizione finale    *
      *    *-----------------------------------------------------------*
       vis-cat-def-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      rr-sel-cat-def       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cat-def-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Categoria - codice iniziale          *
      *    *-----------------------------------------------------------*
       acc-cat-coi-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cat-coi-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-yst-002-ope      .
           move      rr-sel-cat-coi       to   w-cmn-yst-002-cod      .
           move      w-lpt-sel-ain-lin    to   w-cmn-yst-002-lin      .
           move      w-lpt-sel-ain-pos    to   w-cmn-yst-002-pos      .
           move      zero                 to   w-cmn-yst-002-dln      .
           move      zero                 to   w-cmn-yst-002-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cmn-yst-002-cll-000  thru cmn-yst-002-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-yst-002-foi-000  thru cmn-yst-002-foi-999    .
       acc-cat-coi-110.
           perform   cmn-yst-002-cll-000  thru cmn-yst-002-cll-999    .
           if        w-cmn-yst-002-ope    =    "F+"
                     go to acc-cat-coi-115.
           if        w-cmn-yst-002-ope    =    "AC"
                     go to acc-cat-coi-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cat-coi-115.
           perform   cmn-yst-002-foi-000  thru cmn-yst-002-foi-999    .
           go to     acc-cat-coi-110.
       acc-cat-coi-120.
           move      w-cmn-yst-002-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cat-coi-999.
       acc-cat-coi-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-sel-cat-coi         .
       acc-cat-coi-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cat-coi-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cat-coi-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cat-coi-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cat-coi-100.
       acc-cat-coi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Categoria - codice iniziale       *
      *    *-----------------------------------------------------------*
       vis-cat-coi-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      rr-sel-cat-coi       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cat-coi-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Categoria - codice finale            *
      *    *-----------------------------------------------------------*
       acc-cat-cof-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cat-cof-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-yst-002-ope      .
           move      rr-sel-cat-cof       to   w-cmn-yst-002-cod      .
           move      w-lpt-sel-afi-lin    to   w-cmn-yst-002-lin      .
           move      w-lpt-sel-afi-pos    to   w-cmn-yst-002-pos      .
           move      zero                 to   w-cmn-yst-002-dln      .
           move      zero                 to   w-cmn-yst-002-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cmn-yst-002-cll-000  thru cmn-yst-002-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-yst-002-foi-000  thru cmn-yst-002-foi-999    .
       acc-cat-cof-110.
           perform   cmn-yst-002-cll-000  thru cmn-yst-002-cll-999    .
           if        w-cmn-yst-002-ope    =    "F+"
                     go to acc-cat-cof-115.
           if        w-cmn-yst-002-ope    =    "AC"
                     go to acc-cat-cof-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cat-cof-115.
           perform   cmn-yst-002-foi-000  thru cmn-yst-002-foi-999    .
           go to     acc-cat-cof-110.
       acc-cat-cof-120.
           move      w-cmn-yst-002-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cat-cof-999.
       acc-cat-cof-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-sel-cat-cof         .
       acc-cat-cof-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cat-cof-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cat-cof-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cat-cof-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cat-cof-100.
       acc-cat-cof-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Categoria - codice finale         *
      *    *-----------------------------------------------------------*
       vis-cat-cof-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      rr-sel-cat-cof       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cat-cof-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Categoria - mnemonico iniziale       *
      *    *-----------------------------------------------------------*
       acc-cat-mni-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cat-mni-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-cat-mni       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cat-mni-999.
       acc-cat-mni-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-cat-mni         .
       acc-cat-mni-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cat-mni-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cat-mni-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cat-mni-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cat-mni-100.
       acc-cat-mni-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Categoria - mnemonico iniziale    *
      *    *-----------------------------------------------------------*
       vis-cat-mni-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      rr-sel-cat-mni       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cat-mni-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Categoria - mnemonico finale         *
      *    *-----------------------------------------------------------*
       acc-cat-mnf-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cat-mnf-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-cat-mnf       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cat-mnf-999.
       acc-cat-mnf-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-cat-mnf         .
       acc-cat-mnf-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cat-mnf-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cat-mnf-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cat-mnf-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cat-mnf-100.
       acc-cat-mnf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Categoria - mnemonico finale      *
      *    *-----------------------------------------------------------*
       vis-cat-mnf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      rr-sel-cat-mnf       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cat-mnf-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Statistico - descrizione iniziale    *
      *    *-----------------------------------------------------------*
       acc-stt-dei-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-stt-dei-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-stt-dei       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-stt-dei-999.
       acc-stt-dei-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-stt-dei         .
       acc-stt-dei-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-stt-dei-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-stt-dei-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-stt-dei-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-stt-dei-100.
       acc-stt-dei-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Statistico - descrizione iniziale *
      *    *-----------------------------------------------------------*
       vis-stt-dei-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      rr-sel-stt-dei       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-stt-dei-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Statistico - descrizione finale      *
      *    *-----------------------------------------------------------*
       acc-stt-def-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-stt-def-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-stt-def       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-stt-def-999.
       acc-stt-def-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-stt-def         .
       acc-stt-def-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-stt-def-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-stt-def-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-stt-def-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-stt-def-100.
       acc-stt-def-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Statistico - descrizione finale   *
      *    *-----------------------------------------------------------*
       vis-stt-def-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      rr-sel-stt-def       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-stt-def-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Statistico - codice iniziale         *
      *    *-----------------------------------------------------------*
       acc-stt-coi-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-stt-coi-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-yst-003-ope      .
           move      rr-sel-stt-coi       to   w-cmn-yst-003-cod      .
           move      w-lpt-sel-ain-lin    to   w-cmn-yst-003-lin      .
           move      w-lpt-sel-ain-pos    to   w-cmn-yst-003-pos      .
           move      zero                 to   w-cmn-yst-003-dln      .
           move      zero                 to   w-cmn-yst-003-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cmn-yst-003-cll-000  thru cmn-yst-003-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-yst-003-foi-000  thru cmn-yst-003-foi-999    .
       acc-stt-coi-110.
           perform   cmn-yst-003-cll-000  thru cmn-yst-003-cll-999    .
           if        w-cmn-yst-003-ope    =    "F+"
                     go to acc-stt-coi-115.
           if        w-cmn-yst-003-ope    =    "AC"
                     go to acc-stt-coi-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-stt-coi-115.
           perform   cmn-yst-003-foi-000  thru cmn-yst-003-foi-999    .
           go to     acc-stt-coi-110.
       acc-stt-coi-120.
           move      w-cmn-yst-003-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-stt-coi-999.
       acc-stt-coi-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-sel-stt-coi         .
       acc-stt-coi-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-stt-coi-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-stt-coi-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-stt-coi-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-stt-coi-100.
       acc-stt-coi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Statistico - codice iniziale      *
      *    *-----------------------------------------------------------*
       vis-stt-coi-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      rr-sel-stt-coi       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-stt-coi-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Statistico - codice finale           *
      *    *-----------------------------------------------------------*
       acc-stt-cof-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-stt-cof-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-yst-003-ope      .
           move      rr-sel-stt-cof       to   w-cmn-yst-003-cod      .
           move      w-lpt-sel-afi-lin    to   w-cmn-yst-003-lin      .
           move      w-lpt-sel-afi-pos    to   w-cmn-yst-003-pos      .
           move      zero                 to   w-cmn-yst-003-dln      .
           move      zero                 to   w-cmn-yst-003-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cmn-yst-003-cll-000  thru cmn-yst-003-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-yst-003-foi-000  thru cmn-yst-003-foi-999    .
       acc-stt-cof-110.
           perform   cmn-yst-003-cll-000  thru cmn-yst-003-cll-999    .
           if        w-cmn-yst-003-ope    =    "F+"
                     go to acc-stt-cof-115.
           if        w-cmn-yst-003-ope    =    "AC"
                     go to acc-stt-cof-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-stt-cof-115.
           perform   cmn-yst-003-foi-000  thru cmn-yst-003-foi-999    .
           go to     acc-stt-cof-110.
       acc-stt-cof-120.
           move      w-cmn-yst-003-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-stt-cof-999.
       acc-stt-cof-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-sel-stt-cof         .
       acc-stt-cof-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-stt-cof-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-stt-cof-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-stt-cof-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-stt-cof-100.
       acc-stt-cof-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Statistico - codice finale        *
      *    *-----------------------------------------------------------*
       vis-stt-cof-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      rr-sel-stt-cof       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-stt-cof-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Statistico - mnemonico iniziale      *
      *    *-----------------------------------------------------------*
       acc-stt-mni-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-stt-mni-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-stt-mni       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-stt-mni-999.
       acc-stt-mni-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-stt-mni         .
       acc-stt-mni-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-stt-mni-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-stt-mni-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-stt-mni-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-stt-mni-100.
       acc-stt-mni-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Statistico - mnemonico iniziale   *
      *    *-----------------------------------------------------------*
       vis-stt-mni-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      rr-sel-stt-mni       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-stt-mni-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Statistico - mnemonico finale        *
      *    *-----------------------------------------------------------*
       acc-stt-mnf-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-stt-mnf-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-stt-mnf       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-stt-mnf-999.
       acc-stt-mnf-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-stt-mnf         .
       acc-stt-mnf-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-stt-mnf-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-stt-mnf-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-stt-mnf-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-stt-mnf-100.
       acc-stt-mnf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Statistico - mnemonico finale     *
      *    *-----------------------------------------------------------*
       vis-stt-mnf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      rr-sel-stt-mnf       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-stt-mnf-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Pagamento - descrizione iniziale     *
      *    *-----------------------------------------------------------*
       acc-fop-dei-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-fop-dei-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-fop-dei       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-fop-dei-999.
       acc-fop-dei-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-fop-dei         .
       acc-fop-dei-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-fop-dei-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-fop-dei-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-fop-dei-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-fop-dei-100.
       acc-fop-dei-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Pagamento - descrizione iniziale  *
      *    *-----------------------------------------------------------*
       vis-fop-dei-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      rr-sel-fop-dei       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-fop-dei-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Pagamento - descrizione finale       *
      *    *-----------------------------------------------------------*
       acc-fop-def-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-fop-def-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-fop-def       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-fop-def-999.
       acc-fop-def-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-fop-def         .
       acc-fop-def-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-fop-def-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-fop-def-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-fop-def-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-fop-def-100.
       acc-fop-def-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Pagamento - descrizione finale    *
      *    *-----------------------------------------------------------*
       vis-fop-def-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      rr-sel-fop-def       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-fop-def-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Pagamento - codice iniziale          *
      *    *-----------------------------------------------------------*
       acc-fop-coi-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-fop-coi-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-yfp-ope      .
           move      rr-sel-fop-coi       to   w-cod-mne-yfp-cod      .
           move      07                   to   w-cod-mne-yfp-lin      .
           move      40                   to   w-cod-mne-yfp-pos      .
           move      zero                 to   w-cod-mne-yfp-dln      .
           move      zero                 to   w-cod-mne-yfp-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-yfp-cll-000  thru cod-mne-yfp-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-yfp-foi-000  thru cod-mne-yfp-foi-999    .
       acc-fop-coi-110.
           perform   cod-mne-yfp-cll-000  thru cod-mne-yfp-cll-999    .
           if        w-cod-mne-yfp-ope    =    "F+"
                     go to acc-fop-coi-115.
           if        w-cod-mne-yfp-ope    =    "AC"
                     go to acc-fop-coi-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-fop-coi-115.
           perform   cod-mne-yfp-foi-000  thru cod-mne-yfp-foi-999    .
           go to     acc-fop-coi-110.
       acc-fop-coi-120.
           move      w-cod-mne-yfp-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-fop-coi-999.
       acc-fop-coi-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-sel-fop-coi         .
       acc-fop-coi-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-fop-coi-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-fop-coi-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-fop-coi-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-fop-coi-100.
       acc-fop-coi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Pagamento - codice iniziale       *
      *    *-----------------------------------------------------------*
       vis-fop-coi-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      07                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      rr-sel-fop-coi       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-fop-coi-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Pagamento - codice finale            *
      *    *-----------------------------------------------------------*
       acc-fop-cof-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-fop-cof-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-yfp-ope      .
           move      rr-sel-fop-cof       to   w-cod-mne-yfp-cod      .
           move      08                   to   w-cod-mne-yfp-lin      .
           move      40                   to   w-cod-mne-yfp-pos      .
           move      zero                 to   w-cod-mne-yfp-dln      .
           move      zero                 to   w-cod-mne-yfp-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-yfp-cll-000  thru cod-mne-yfp-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-yfp-foi-000  thru cod-mne-yfp-foi-999    .
       acc-fop-cof-110.
           perform   cod-mne-yfp-cll-000  thru cod-mne-yfp-cll-999    .
           if        w-cod-mne-yfp-ope    =    "F+"
                     go to acc-fop-cof-115.
           if        w-cod-mne-yfp-ope    =    "AC"
                     go to acc-fop-cof-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-fop-cof-115.
           perform   cod-mne-yfp-foi-000  thru cod-mne-yfp-foi-999    .
           go to     acc-fop-cof-110.
       acc-fop-cof-120.
           move      w-cod-mne-yfp-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-fop-cof-999.
       acc-fop-cof-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-sel-fop-cof         .
       acc-fop-cof-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-fop-cof-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-fop-cof-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-fop-cof-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-fop-cof-100.
       acc-fop-cof-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Pagamento - codice finale         *
      *    *-----------------------------------------------------------*
       vis-fop-cof-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      08                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      rr-sel-fop-cof       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-fop-cof-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Pagamento - mnemonico iniziale       *
      *    *-----------------------------------------------------------*
       acc-fop-mni-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-fop-mni-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-fop-mni       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-fop-mni-999.
       acc-fop-mni-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-fop-mni         .
       acc-fop-mni-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-fop-mni-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-fop-mni-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-fop-mni-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-fop-mni-100.
       acc-fop-mni-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Pagamento - mnemonico iniziale    *
      *    *-----------------------------------------------------------*
       vis-fop-mni-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      rr-sel-fop-mni       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-fop-mni-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Pagamento - mnemonico finale         *
      *    *-----------------------------------------------------------*
       acc-fop-mnf-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-fop-mnf-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-fop-mnf       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-fop-mnf-999.
       acc-fop-mnf-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-fop-mnf         .
       acc-fop-mnf-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-fop-mnf-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-fop-mnf-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-fop-mnf-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-fop-mnf-100.
       acc-fop-mnf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Pagamento - mnemonico finale      *
      *    *-----------------------------------------------------------*
       vis-fop-mnf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      rr-sel-fop-mnf       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-fop-mnf-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Nazione - descrizione iniziale       *
      *    *-----------------------------------------------------------*
       acc-naz-dei-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-naz-dei-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-naz-dei       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-naz-dei-999.
       acc-naz-dei-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-naz-dei         .
       acc-naz-dei-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-naz-dei-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-naz-dei-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-naz-dei-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-naz-dei-100.
       acc-naz-dei-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Nazione - descrizione iniziale    *
      *    *-----------------------------------------------------------*
       vis-naz-dei-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      rr-sel-naz-dei       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-naz-dei-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Nazione - descrizione finale         *
      *    *-----------------------------------------------------------*
       acc-naz-def-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-naz-def-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-naz-def       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-naz-def-999.
       acc-naz-def-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-naz-def         .
       acc-naz-def-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-naz-def-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-naz-def-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-naz-def-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-naz-def-100.
       acc-naz-def-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Nazione - descrizione finale      *
      *    *-----------------------------------------------------------*
       vis-naz-def-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      rr-sel-naz-def       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-naz-def-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Nazione - codice iniziale            *
      *    *-----------------------------------------------------------*
       acc-naz-coi-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-naz-coi-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-naz-ope      .
           move      rr-sel-naz-coi       to   w-cod-des-naz-cod      .
           move      w-lpt-sel-ain-lin    to   w-cod-des-naz-lin      .
           move      w-lpt-sel-ain-pos    to   w-cod-des-naz-pos      .
           move      zero                 to   w-cod-des-naz-dln      .
           move      zero                 to   w-cod-des-naz-dps      .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-des-naz-cll-000  thru cod-des-naz-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-naz-foi-000  thru cod-des-naz-foi-999    .
       acc-naz-coi-110.
           perform   cod-des-naz-cll-000  thru cod-des-naz-cll-999    .
           if        w-cod-des-naz-ope    =    "F+"
                     go to acc-naz-coi-115.
           if        w-cod-des-naz-ope    =    "AC"
                     go to acc-naz-coi-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-naz-coi-115.
           perform   cod-des-naz-foi-000  thru cod-des-naz-foi-999    .
           go to     acc-naz-coi-110.
       acc-naz-coi-120.
           move      w-cod-des-naz-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-naz-coi-999.
       acc-naz-coi-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-naz-coi         .
       acc-naz-coi-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-naz-coi-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-naz-coi-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-naz-coi-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-naz-coi-100.
       acc-naz-coi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Nazione - codice iniziale         *
      *    *-----------------------------------------------------------*
       vis-naz-coi-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      rr-sel-naz-coi       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-naz-coi-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Nazione - codice finale              *
      *    *-----------------------------------------------------------*
       acc-naz-cof-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-naz-cof-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-naz-ope      .
           move      rr-sel-naz-cof       to   w-cod-des-naz-cod      .
           move      w-lpt-sel-afi-lin    to   w-cod-des-naz-lin      .
           move      w-lpt-sel-afi-pos    to   w-cod-des-naz-pos      .
           move      zero                 to   w-cod-des-naz-dln      .
           move      zero                 to   w-cod-des-naz-dps      .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-des-naz-cll-000  thru cod-des-naz-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-naz-foi-000  thru cod-des-naz-foi-999    .
       acc-naz-cof-110.
           perform   cod-des-naz-cll-000  thru cod-des-naz-cll-999    .
           if        w-cod-des-naz-ope    =    "F+"
                     go to acc-naz-cof-115.
           if        w-cod-des-naz-ope    =    "AC"
                     go to acc-naz-cof-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-naz-cof-115.
           perform   cod-des-naz-foi-000  thru cod-des-naz-foi-999    .
           go to     acc-naz-cof-110.
       acc-naz-cof-120.
           move      w-cod-des-naz-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-naz-cof-999.
       acc-naz-cof-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-naz-cof         .
       acc-naz-cof-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-naz-cof-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-naz-cof-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-naz-cof-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-naz-cof-100.
       acc-naz-cof-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Nazione - codice finale           *
      *    *-----------------------------------------------------------*
       vis-naz-cof-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      rr-sel-naz-cof       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-naz-cof-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Regione - descrizione iniziale       *
      *    *-----------------------------------------------------------*
       acc-rgn-dei-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-rgn-dei-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      25                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-rgn-dei       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-rgn-dei-999.
       acc-rgn-dei-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-rgn-dei         .
       acc-rgn-dei-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-rgn-dei-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-rgn-dei-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-rgn-dei-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-rgn-dei-100.
       acc-rgn-dei-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Regione - descrizione iniziale    *
      *    *-----------------------------------------------------------*
       vis-rgn-dei-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      25                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      rr-sel-rgn-dei       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rgn-dei-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Regione - descrizione finale         *
      *    *-----------------------------------------------------------*
       acc-rgn-def-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-rgn-def-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      25                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-rgn-def       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-rgn-def-999.
       acc-rgn-def-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-rgn-def         .
       acc-rgn-def-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-rgn-def-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-rgn-def-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-rgn-def-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-rgn-def-100.
       acc-rgn-def-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Regione - descrizione finale      *
      *    *-----------------------------------------------------------*
       vis-rgn-def-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      25                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      rr-sel-rgn-def       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rgn-def-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Regione - codice iniziale            *
      *    *-----------------------------------------------------------*
       acc-rgn-coi-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-rgn-coi-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-rgn-ope      .
           move      rr-sel-rgn-coi       to   w-cod-des-rgn-cod      .
           move      w-lpt-sel-ain-lin    to   w-cod-des-rgn-lin      .
           move      w-lpt-sel-ain-pos    to   w-cod-des-rgn-pos      .
           move      zero                 to   w-cod-des-rgn-dln      .
           move      zero                 to   w-cod-des-rgn-dps      .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-des-rgn-cll-000  thru cod-des-rgn-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-rgn-foi-000  thru cod-des-rgn-foi-999    .
       acc-rgn-coi-110.
           perform   cod-des-rgn-cll-000  thru cod-des-rgn-cll-999    .
           if        w-cod-des-rgn-ope    =    "F+"
                     go to acc-rgn-coi-115.
           if        w-cod-des-rgn-ope    =    "AC"
                     go to acc-rgn-coi-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-rgn-coi-115.
           perform   cod-des-rgn-foi-000  thru cod-des-rgn-foi-999    .
           go to     acc-rgn-coi-110.
       acc-rgn-coi-120.
           move      w-cod-des-rgn-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-rgn-coi-999.
       acc-rgn-coi-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-rgn-coi         .
       acc-rgn-coi-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-rgn-coi-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-rgn-coi-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-rgn-coi-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-rgn-coi-100.
       acc-rgn-coi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Regione - codice iniziale         *
      *    *-----------------------------------------------------------*
       vis-rgn-coi-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      rr-sel-rgn-coi       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rgn-coi-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Regione - codice finale              *
      *    *-----------------------------------------------------------*
       acc-rgn-cof-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-rgn-cof-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-rgn-ope      .
           move      rr-sel-rgn-cof       to   w-cod-des-rgn-cod      .
           move      w-lpt-sel-afi-lin    to   w-cod-des-rgn-lin      .
           move      w-lpt-sel-afi-pos    to   w-cod-des-rgn-pos      .
           move      zero                 to   w-cod-des-rgn-dln      .
           move      zero                 to   w-cod-des-rgn-dps      .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-des-rgn-cll-000  thru cod-des-rgn-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-rgn-foi-000  thru cod-des-rgn-foi-999    .
       acc-rgn-cof-110.
           perform   cod-des-rgn-cll-000  thru cod-des-rgn-cll-999    .
           if        w-cod-des-rgn-ope    =    "F+"
                     go to acc-rgn-cof-115.
           if        w-cod-des-rgn-ope    =    "AC"
                     go to acc-rgn-cof-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-rgn-cof-115.
           perform   cod-des-rgn-foi-000  thru cod-des-rgn-foi-999    .
           go to     acc-rgn-cof-110.
       acc-rgn-cof-120.
           move      w-cod-des-rgn-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-rgn-cof-999.
       acc-rgn-cof-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-rgn-cof         .
       acc-rgn-cof-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-rgn-cof-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-rgn-cof-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-rgn-cof-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-rgn-cof-100.
       acc-rgn-cof-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Regione - codice finale           *
      *    *-----------------------------------------------------------*
       vis-rgn-cof-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      rr-sel-rgn-cof       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rgn-cof-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Provincia - descrizione iniziale     *
      *    *-----------------------------------------------------------*
       acc-prv-dei-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-prv-dei-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      25                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-prv-dei       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-prv-dei-999.
       acc-prv-dei-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-prv-dei         .
       acc-prv-dei-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-prv-dei-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-prv-dei-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-prv-dei-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-prv-dei-100.
       acc-prv-dei-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Provincia - descrizione iniziale  *
      *    *-----------------------------------------------------------*
       vis-prv-dei-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      25                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      rr-sel-prv-dei       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prv-dei-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Provincia - descrizione finale       *
      *    *-----------------------------------------------------------*
       acc-prv-def-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-prv-def-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      25                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-prv-def       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-prv-def-999.
       acc-prv-def-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-prv-def         .
       acc-prv-def-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-prv-def-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-prv-def-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-prv-def-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-prv-def-100.
       acc-prv-def-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Provincia - descrizione finale    *
      *    *-----------------------------------------------------------*
       vis-prv-def-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      25                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      rr-sel-prv-def       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prv-def-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Provincia - codice iniziale          *
      *    *-----------------------------------------------------------*
       acc-prv-coi-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-prv-coi-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-prv-ope      .
           move      rr-sel-prv-coi       to   w-cod-des-prv-cod      .
           move      w-lpt-sel-ain-lin    to   w-cod-des-prv-lin      .
           move      w-lpt-sel-ain-pos    to   w-cod-des-prv-pos      .
           move      zero                 to   w-cod-des-prv-dln      .
           move      zero                 to   w-cod-des-prv-dps      .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-des-prv-cll-000  thru cod-des-prv-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-prv-foi-000  thru cod-des-prv-foi-999    .
       acc-prv-coi-110.
           perform   cod-des-prv-cll-000  thru cod-des-prv-cll-999    .
           if        w-cod-des-prv-ope    =    "F+"
                     go to acc-prv-coi-115.
           if        w-cod-des-prv-ope    =    "AC"
                     go to acc-prv-coi-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-prv-coi-115.
           perform   cod-des-prv-foi-000  thru cod-des-prv-foi-999    .
           go to     acc-prv-coi-110.
       acc-prv-coi-120.
           move      w-cod-des-prv-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-prv-coi-999.
       acc-prv-coi-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-prv-coi         .
       acc-prv-coi-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-prv-coi-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-prv-coi-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-prv-coi-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-prv-coi-100.
       acc-prv-coi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Provincia - codice iniziale       *
      *    *-----------------------------------------------------------*
       vis-prv-coi-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      rr-sel-prv-coi       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prv-coi-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Provincia - codice finale            *
      *    *-----------------------------------------------------------*
       acc-prv-cof-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-prv-cof-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-prv-ope      .
           move      rr-sel-prv-cof       to   w-cod-des-prv-cod      .
           move      w-lpt-sel-afi-lin    to   w-cod-des-prv-lin      .
           move      w-lpt-sel-afi-pos    to   w-cod-des-prv-pos      .
           move      zero                 to   w-cod-des-prv-dln      .
           move      zero                 to   w-cod-des-prv-dps      .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-des-prv-cll-000  thru cod-des-prv-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-prv-foi-000  thru cod-des-prv-foi-999    .
       acc-prv-cof-110.
           perform   cod-des-prv-cll-000  thru cod-des-prv-cll-999    .
           if        w-cod-des-prv-ope    =    "F+"
                     go to acc-prv-cof-115.
           if        w-cod-des-prv-ope    =    "AC"
                     go to acc-prv-cof-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-prv-cof-115.
           perform   cod-des-prv-foi-000  thru cod-des-prv-foi-999    .
           go to     acc-prv-cof-110.
       acc-prv-cof-120.
           move      w-cod-des-prv-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-prv-cof-999.
       acc-prv-cof-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-prv-cof         .
       acc-prv-cof-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-prv-cof-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-prv-cof-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-prv-cof-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-prv-cof-100.
       acc-prv-cof-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Provincia - codice finale         *
      *    *-----------------------------------------------------------*
       vis-prv-cof-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      rr-sel-prv-cof       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prv-cof-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Comune - descrizione iniziale        *
      *    *-----------------------------------------------------------*
       acc-cmn-dei-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cmn-dei-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-cmn-dei       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cmn-dei-999.
       acc-cmn-dei-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-cmn-dei         .
       acc-cmn-dei-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cmn-dei-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cmn-dei-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cmn-dei-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cmn-dei-100.
       acc-cmn-dei-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Comune - descrizione iniziale     *
      *    *-----------------------------------------------------------*
       vis-cmn-dei-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      rr-sel-cmn-dei       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cmn-dei-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Comune - descrizione finale          *
      *    *-----------------------------------------------------------*
       acc-cmn-def-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cmn-def-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-cmn-def       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cmn-def-999.
       acc-cmn-def-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-cmn-def         .
       acc-cmn-def-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cmn-def-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cmn-def-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cmn-def-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cmn-def-100.
       acc-cmn-def-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Comune - descrizione finale       *
      *    *-----------------------------------------------------------*
       vis-cmn-def-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      rr-sel-cmn-def       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cmn-def-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Comune - C.a.p. iniziale             *
      *    *-----------------------------------------------------------*
       acc-cmn-cpi-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cmn-cpi-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-cmn-cpi       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cmn-cpi-999.
       acc-cmn-cpi-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-cmn-cpi         .
       acc-cmn-cpi-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cmn-cpi-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cmn-cpi-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cmn-cpi-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cmn-cpi-100.
       acc-cmn-cpi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Comune - C.a.p. iniziale          *
      *    *-----------------------------------------------------------*
       vis-cmn-cpi-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-lpt-sel-ain-lin    to   v-lin                  .
           move      w-lpt-sel-ain-pos    to   v-pos                  .
           move      rr-sel-cmn-cpi       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cmn-cpi-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Comune - C.a.p. finale               *
      *    *-----------------------------------------------------------*
       acc-cmn-cpf-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cmn-cpf-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sel-cmn-cpf       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cmn-cpf-999.
       acc-cmn-cpf-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-sel-cmn-cpf         .
       acc-cmn-cpf-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cmn-cpf-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cmn-cpf-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cmn-cpf-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cmn-cpf-100.
       acc-cmn-cpf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Comune - C.a.p. finale            *
      *    *-----------------------------------------------------------*
       vis-cmn-cpf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-lpt-sel-afi-lin    to   v-lin                  .
           move      w-lpt-sel-afi-pos    to   v-pos                  .
           move      rr-sel-cmn-cpf       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cmn-cpf-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo ordinamento fornitori a parita' *
      *    *                      anagrafica di ordinamento            *
      *    *-----------------------------------------------------------*
       acc-tip-ocp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il tipo ordinamento stampa e' per    *
      *                      * fornitore, il campo non deve essere ac- *
      *                      * cettato                                 *
      *                      *-----------------------------------------*
           if        rr-tip-ors           =    01
                     go to acc-tip-ocp-999.
       acc-tip-ocp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ocp-lun    to   v-car                  .
           move      w-exp-tip-ocp-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-lpt-tip-ocp-lin    to   v-lin                  .
           move      w-lpt-tip-ocp-pos    to   v-pos                  .
           move      w-exp-tip-ocp-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-tip-ocp           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-ocp-999.
       acc-tip-ocp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tip-ocp             .
       acc-tip-ocp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-tip-ocp           =    zero
                     go to acc-tip-ocp-100.
       acc-tip-ocp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-ocp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-ocp-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-ocp-100.
       acc-tip-ocp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo ordinamento fornitori a pa-  *
      *    *                         rita' di anagrafica di ordinamen- *
      *    *                         to                                *
      *    *-----------------------------------------------------------*
       vis-tip-ocp-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ocp-lun    to   v-car                  .
           move      w-exp-tip-ocp-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-lpt-tip-ocp-lin    to   v-lin                  .
           move      w-lpt-tip-ocp-pos    to   v-pos                  .
           move      w-exp-tip-ocp-tbl    to   v-txt                  .
           move      rr-tip-ocp           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-ocp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo di stampa richiesta             *
      *    *-----------------------------------------------------------*
       acc-tip-stp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore attuale                  *
      *                  *---------------------------------------------*
           move      rr-tip-stp           to   w-sav-tip-stp          .
       acc-tip-stp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-stp-lun    to   v-car                  .
           move      w-exp-tip-stp-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      w-exp-tip-stp-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
      *
           if        rr-tip-stp           =    01
                     move  01             to   v-num
           else if   rr-tip-stp           =    11
                     move  02             to   v-num
           else if   rr-tip-stp           =    21
                     move  03             to   v-num
           else if   rr-tip-stp           =    31
                     move  04             to   v-num
           else if   rr-tip-stp           =    32
                     move  05             to   v-num
           else if   rr-tip-stp           =    51
                     move  06             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-stp-999.
       acc-tip-stp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  01             to   rr-tip-stp
           else if   v-num                =    02
                     move  11             to   rr-tip-stp
           else if   v-num                =    03
                     move  21             to   rr-tip-stp
           else if   v-num                =    04
                     move  31             to   rr-tip-stp
           else if   v-num                =    05
                     move  32             to   rr-tip-stp
           else if   v-num                =    06
                     move  51             to   rr-tip-stp
           else      move  zero           to   rr-tip-stp             .
       acc-tip-stp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-tip-stp           not  = 01 and
                     rr-tip-stp           not  = 11 and
                     rr-tip-stp           not  = 21 and
                     rr-tip-stp           not  = 31 and
                     rr-tip-stp           not  = 32 and
                     rr-tip-stp           not  = 51
                     go to acc-tip-stp-100.
       acc-tip-stp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-stp-625.
      *                  *---------------------------------------------*
      *                  * Eventuale normalizzazione si/no salto pagi- *
      *                  * na a rottura                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        rr-tip-stp           not  = 21 and
                     rr-tip-stp           not  = 31 and
                     rr-tip-stp           not  = 32 and
                     rr-tip-stp           not  = 51
                     go to acc-tip-stp-630.
           if        rr-sns-prt           =    zero
                     go to acc-tip-stp-630.
      *                      *-----------------------------------------*
      *                      * Normalizzazione                         *
      *                      *-----------------------------------------*
           move      zero                 to   rr-sns-prt             .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           perform   vis-sns-prt-000      thru vis-sns-prt-999        .
       acc-tip-stp-630.
      *                  *---------------------------------------------*
      *                  * Eventuale normalizzazione si/no stampa      *
      *                  * contatori elementi                          *
      *                  *---------------------------------------------*
       acc-tip-stp-650.
      *                  *---------------------------------------------*
      *                  * Eventuale normalizzazione voci aggiuntive   *
      *                  * da stampare                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        rr-tip-stp           not  = 01 and
                     rr-tip-stp           not  = 21 and
                     rr-tip-stp           not  = 31 and
                     rr-tip-stp           not  = 32
                     go to acc-tip-stp-660.
      *                      *-----------------------------------------*
      *                      * Normalizzazione                         *
      *                      *-----------------------------------------*
           move      spaces               to   rr-vag-dst             .
       acc-tip-stp-660.
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           perform   pmt-vag-dst-000      thru pmt-vag-dst-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           perform   vis-vag-dst-000      thru vis-vag-dst-999        .
       acc-tip-stp-675.
      *                  *---------------------------------------------*
      *                  * Eventuale normalizzazione tipo di etichette *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        rr-tip-stp           not  = 01 and
                     rr-tip-stp           not  = 11 and
                     rr-tip-stp           not  = 21 and
                     rr-tip-stp           not  = 51
                     go to acc-tip-stp-680.
      *                      *-----------------------------------------*
      *                      * Normalizzazione                         *
      *                      *-----------------------------------------*
           move      zero                 to   rr-tip-eti             .
       acc-tip-stp-680.
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           perform   pmt-tip-eti-000      thru pmt-tip-eti-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           perform   vis-tip-eti-000      thru vis-tip-eti-999        .
       acc-tip-stp-695.
      *                  *---------------------------------------------*
      *                  * Fine dipendenze da impostazione             *
      *                  *---------------------------------------------*
           go to     acc-tip-stp-800.
       acc-tip-stp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-stp-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-stp-100.
       acc-tip-stp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo di stampa richiesta          *
      *    *-----------------------------------------------------------*
       vis-tip-stp-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-stp-lun    to   v-car                  .
           move      w-exp-tip-stp-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      w-exp-tip-stp-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
      *
           if        rr-tip-stp           =    01
                     move  01             to   v-num
           else if   rr-tip-stp           =    11
                     move  02             to   v-num
           else if   rr-tip-stp           =    21
                     move  03             to   v-num
           else if   rr-tip-stp           =    31
                     move  04             to   v-num
           else if   rr-tip-stp           =    32
                     move  05             to   v-num
           else if   rr-tip-stp           =    51
                     move  06             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-stp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Voci aggiuntive da stampare          *
      *    *-----------------------------------------------------------*
       acc-vag-dst-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-vag-dst-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-stp           not  = 11 and
                     rr-tip-stp           not  = 51
                     go to acc-vag-dst-999.
       acc-vag-dst-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "C"                  to   v-tip                  .
           move      w-exp-vag-dst-lun    to   v-car                  .
           move      w-exp-vag-dst-num    to   v-ldt                  .
           move      "X"                  to   v-edm                  .
           move      w-lpt-vag-dst-lin    to   v-lin                  .
           move      w-lpt-vag-dst-pos    to   v-pos                  .
           move      w-exp-vag-dst-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-vag-dst           to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-vag-dst-999.
       acc-vag-dst-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-vag-dst             .
       acc-vag-dst-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-vag-dst-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-vag-dst-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-vag-dst-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-vag-dst-100.
       acc-vag-dst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Voci aggiuntive da stampare       *
      *    *-----------------------------------------------------------*
       vis-vag-dst-000.
           move      "DS"                 to   v-ope                  .
           move      "C"                  to   v-tip                  .
           move      w-exp-vag-dst-lun    to   v-car                  .
           move      w-exp-vag-dst-num    to   v-ldt                  .
           move      "B"                  to   v-edm                  .
           move      w-lpt-vag-dst-lin    to   v-lin                  .
           move      w-lpt-vag-dst-pos    to   v-pos                  .
           move      w-exp-vag-dst-tbl    to   v-txt                  .
           move      rr-vag-dst           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-vag-dst-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Si/no salto pagina a rottura         *
      *    *-----------------------------------------------------------*
       acc-sns-prt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-sns-prt-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il tipo ordinamento stampa e' per    *
      *                      * fornitore, il campo non deve essere ac- *
      *                      * cettato                                 *
      *                      *-----------------------------------------*
           if        rr-tip-ors           =    01
                     go to acc-sns-prt-999.
      *                      *-----------------------------------------*
      *                      * Se il tipo stampa e' scheda completa    *
      *                      * fornitore o etichette, il campo non     *
      *                      * deve essere accettato                   *
      *                      *-----------------------------------------*
           if        rr-tip-stp           =    21 or
                     rr-tip-stp           =    31 or
                     rr-tip-stp           =    32 or
                     rr-tip-stp           =    51
                     go to acc-sns-prt-999.
       acc-sns-prt-050.
      *                  *---------------------------------------------*
      *                  * Eventuale preparazione del default          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il tipo ordinamento stampa e' per    *
      *                      * codice di avviamento postale si prepa-  *
      *                      * ra il default a No                      *
      *                      *-----------------------------------------*
           if        rr-tip-ors           =    21 or
                     rr-tip-ors           =    22
                     move  02             to   rr-sns-prt             .
      *                      *-----------------------------------------*
      *                      * Se il tipo ordinamento stampa e' per    *
      *                      * comune postale si prepara il default a  *
      *                      * No                                      *
      *                      *-----------------------------------------*
           if        rr-tip-ors           =    14
                     move  02             to   rr-sns-prt             .
       acc-sns-prt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sns-prt-lun    to   v-car                  .
           move      w-exp-sns-prt-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-lpt-sns-prt-lin    to   v-lin                  .
           move      w-lpt-sns-prt-pos    to   v-pos                  .
           move      w-exp-sns-prt-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sns-prt           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-sns-prt-999.
       acc-sns-prt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-sns-prt             .
       acc-sns-prt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-sns-prt           =    zero
                     go to acc-sns-prt-100.
       acc-sns-prt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sns-prt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-sns-prt-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-sns-prt-100.
       acc-sns-prt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Si/no salto pagina a rottura      *
      *    *-----------------------------------------------------------*
       vis-sns-prt-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sns-prt-lun    to   v-car                  .
           move      w-exp-sns-prt-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-lpt-sns-prt-lin    to   v-lin                  .
           move      w-lpt-sns-prt-pos    to   v-pos                  .
           move      w-exp-sns-prt-tbl    to   v-txt                  .
           move      rr-sns-prt           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sns-prt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo di etichette                    *
      *    *-----------------------------------------------------------*
       acc-tip-eti-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-eti-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-stp           not  = 31 and
                     rr-tip-stp           not  = 32
                     go to acc-tip-eti-999.
       acc-tip-eti-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-eti-lun    to   v-car                  .
           move      w-exp-tip-eti-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      w-exp-tip-eti-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
      *
           if        rr-tip-eti           =    01
                     move  01             to   v-num
           else if   rr-tip-eti           =    51
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-eti-999.
       acc-tip-eti-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  01             to   rr-tip-eti
           else if   v-num                =    02
                     move  51             to   rr-tip-eti
           else      move  zero           to   rr-tip-eti             .
       acc-tip-eti-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-tip-eti           not  = 01 and
                     rr-tip-eti           not  = 51
                     go to acc-tip-eti-100.
       acc-tip-eti-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-eti-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-eti-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-eti-100.
       acc-tip-eti-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo di etichette                 *
      *    *-----------------------------------------------------------*
       vis-tip-eti-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-eti-lun    to   v-car                  .
           move      w-exp-tip-eti-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      w-exp-tip-eti-tbl    to   v-txt                  .
      *
           if        rr-tip-eti           =    01
                     move  01             to   v-num
           else if   rr-tip-eti           =    51
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-eti-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice nostra banca                        *
      *    *-----------------------------------------------------------*
       acc-cod-cbp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-cbp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-cbp-ope      .
           move      02                   to   w-cod-des-cbp-tip      .
           move      rr-ssu-cod-cbp       to   w-cod-des-cbp-cod      .
           move      17                   to   w-cod-des-cbp-lin      .
           move      18                   to   w-cod-des-cbp-pos      .
           move      17                   to   w-cod-des-cbp-dln      .
           move      30                   to   w-cod-des-cbp-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-des-cbp-cll-000  thru cod-des-cbp-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-cbp-foi-000  thru cod-des-cbp-foi-999    .
       acc-cod-cbp-110.
           perform   cod-des-cbp-cll-000  thru cod-des-cbp-cll-999    .
           if        w-cod-des-cbp-ope    =    "F+"
                     go to acc-cod-cbp-115.
           if        w-cod-des-cbp-ope    =    "AC"
                     go to acc-cod-cbp-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-cbp-115.
           perform   cod-des-cbp-foi-000  thru cod-des-cbp-foi-999    .
           go to     acc-cod-cbp-110.
       acc-cod-cbp-120.
           move      w-cod-des-cbp-cod    to   v-alf                  .
       acc-cod-cbp-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-cbp-999.
       acc-cod-cbp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-ssu-cod-cbp         .
       acc-cod-cbp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura file [cbp]                          *
      *                  *---------------------------------------------*
           move      02                   to   w-let-arc-cbp-tip      .
           move      rr-ssu-cod-cbp       to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           if        rr-ssu-cod-cbp       =    spaces
                     move  "Tutte"        to   rr-ssu-cod-cbp-des
           else      move  w-let-arc-cbp-des
                                          to   rr-ssu-cod-cbp-des     .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cod-cbp-des-000  thru vis-cod-cbp-des-999    .
      *                  *---------------------------------------------*
      *                  * Se record non esistente : reimpostazione    *
      *                  *---------------------------------------------*
           if        w-let-arc-cbp-flg    not  = spaces
                     go to acc-cod-cbp-100.
       acc-cod-cbp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-cbp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-cbp-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-cbp-100.
       acc-cod-cbp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice nostra banca                     *
      *    *-----------------------------------------------------------*
       vis-cod-cbp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      18                   to   v-pos                  .
           move      rr-ssu-cod-cbp       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cbp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice nostra banca, descrizione        *
      *    *-----------------------------------------------------------*
       vis-cod-cbp-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-ssu-cod-cbp-des   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cbp-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice zona                          *
      *    *-----------------------------------------------------------*
       acc-cod-zon-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il tipo ordinamento stampa e' per    *
      *                      * zona, il campo non deve essere accet-   *
      *                      * tato                                    *
      *                      *-----------------------------------------*
           if        rr-tip-ors           =    03
                     go to acc-cod-zon-999.
       acc-cod-zon-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-yst-001-ope      .
           move      rr-ssu-cod-zon       to   w-cmn-yst-001-cod      .
           move      18                   to   w-cmn-yst-001-lin      .
           move      18                   to   w-cmn-yst-001-pos      .
           move      18                   to   w-cmn-yst-001-dln      .
           move      30                   to   w-cmn-yst-001-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cmn-yst-001-cll-000  thru cmn-yst-001-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-yst-001-foi-000  thru cmn-yst-001-foi-999    .
       acc-cod-zon-110.
           perform   cmn-yst-001-cll-000  thru cmn-yst-001-cll-999    .
           if        w-cmn-yst-001-ope    =    "F+"
                     go to acc-cod-zon-115.
           if        w-cmn-yst-001-ope    =    "AC"
                     go to acc-cod-zon-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-zon-115.
           perform   cmn-yst-001-foi-000  thru cmn-yst-001-foi-999    .
           go to     acc-cod-zon-110.
       acc-cod-zon-120.
           move      w-cmn-yst-001-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-zon-999.
       acc-cod-zon-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-ssu-cod-zon         .
       acc-cod-zon-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      01                   to   w-let-arc-yst-tip      .
           move      rr-ssu-cod-zon       to   w-let-arc-yst-cod      .
           perform   let-arc-yst-000      thru let-arc-yst-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           if        rr-ssu-cod-zon       =    zero
                     move  "Tutte               "
                                          to   rr-ssu-cod-zon-des
           else      move  w-let-arc-yst-des
                                          to   rr-ssu-cod-zon-des     .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cod-zon-des-000  thru vis-cod-zon-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-yst-flg    not  = spaces
                     go to acc-cod-zon-100.
       acc-cod-zon-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-zon-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-zon-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-zon-100.
       acc-cod-zon-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice zona                       *
      *    *-----------------------------------------------------------*
       vis-cod-zon-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      18                   to   v-lin                  .
           move      18                   to   v-pos                  .
           move      rr-ssu-cod-zon       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-zon-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione zona                  *
      *    *-----------------------------------------------------------*
       vis-cod-zon-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-ssu-cod-zon-des   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-zon-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Categoria fornitori                  *
      *    *-----------------------------------------------------------*
       acc-cod-cat-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il tipo ordinamento stampa e' per    *
      *                      * categoria, il campo non deve essere ac- *
      *                      * cettato                                 *
      *                      *-----------------------------------------*
           if        rr-tip-ors           =    04
                     go to acc-cod-cat-999.
       acc-cod-cat-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-yst-002-ope      .
           move      rr-ssu-cod-cat       to   w-cmn-yst-002-cod      .
           move      19                   to   w-cmn-yst-002-lin      .
           move      18                   to   w-cmn-yst-002-pos      .
           move      19                   to   w-cmn-yst-002-dln      .
           move      30                   to   w-cmn-yst-002-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cmn-yst-002-cll-000  thru cmn-yst-002-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-yst-002-foi-000  thru cmn-yst-002-foi-999    .
       acc-cod-cat-110.
           perform   cmn-yst-002-cll-000  thru cmn-yst-002-cll-999    .
           if        w-cmn-yst-002-ope    =    "F+"
                     go to acc-cod-cat-115.
           if        w-cmn-yst-002-ope    =    "AC"
                     go to acc-cod-cat-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-cat-115.
           perform   cmn-yst-002-foi-000  thru cmn-yst-002-foi-999    .
           go to     acc-cod-cat-110.
       acc-cod-cat-120.
           move      w-cmn-yst-002-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-cat-999.
       acc-cod-cat-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-ssu-cod-cat         .
       acc-cod-cat-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      02                   to   w-let-arc-yst-tip      .
           move      rr-ssu-cod-cat       to   w-let-arc-yst-cod      .
           perform   let-arc-yst-000      thru let-arc-yst-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           if        rr-ssu-cod-cat       =    zero
                     move  "Tutte               "
                                          to   rr-ssu-cod-cat-des
           else      move  w-let-arc-yst-des
                                          to   rr-ssu-cod-cat-des     .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cod-cat-des-000  thru vis-cod-cat-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-yst-flg    not  = spaces
                     go to acc-cod-cat-100.
       acc-cod-cat-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-cat-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-cat-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-cat-100.
       acc-cod-cat-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice categoria                  *
      *    *-----------------------------------------------------------*
       vis-cod-cat-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      19                   to   v-lin                  .
           move      18                   to   v-pos                  .
           move      rr-ssu-cod-cat       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cat-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione categoria             *
      *    *-----------------------------------------------------------*
       vis-cod-cat-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-ssu-cod-cat-des   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cat-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice statistico                    *
      *    *-----------------------------------------------------------*
       acc-cod-stt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il tipo ordinamento stampa e' per    *
      *                      * codice statistico, il campo non deve    *
      *                      * essere accettato                        *
      *                      *-----------------------------------------*
           if        rr-tip-ors           =    05
                     go to acc-cod-stt-999.
       acc-cod-stt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-yst-003-ope      .
           move      rr-ssu-cod-stt       to   w-cmn-yst-003-cod      .
           move      20                   to   w-cmn-yst-003-lin      .
           move      18                   to   w-cmn-yst-003-pos      .
           move      20                   to   w-cmn-yst-003-dln      .
           move      30                   to   w-cmn-yst-003-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cmn-yst-003-cll-000  thru cmn-yst-003-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-yst-003-foi-000  thru cmn-yst-003-foi-999    .
       acc-cod-stt-110.
           perform   cmn-yst-003-cll-000  thru cmn-yst-003-cll-999    .
           if        w-cmn-yst-003-ope    =    "F+"
                     go to acc-cod-stt-115.
           if        w-cmn-yst-003-ope    =    "AC"
                     go to acc-cod-stt-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-stt-115.
           perform   cmn-yst-003-foi-000  thru cmn-yst-003-foi-999    .
           go to     acc-cod-stt-110.
       acc-cod-stt-120.
           move      w-cmn-yst-003-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-stt-999.
       acc-cod-stt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-ssu-cod-stt         .
       acc-cod-stt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      03                   to   w-let-arc-yst-tip      .
           move      rr-ssu-cod-stt       to   w-let-arc-yst-cod      .
           perform   let-arc-yst-000      thru let-arc-yst-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           if        rr-ssu-cod-stt       =    zero
                     move  "Tutti               "
                                          to   rr-ssu-cod-stt-des
           else      move  w-let-arc-yst-des
                                          to   rr-ssu-cod-stt-des     .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cod-stt-des-000  thru vis-cod-stt-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-yst-flg    not  = spaces
                     go to acc-cod-stt-100.
       acc-cod-stt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-stt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-stt-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-stt-100.
       acc-cod-stt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice statistico                 *
      *    *-----------------------------------------------------------*
       vis-cod-stt-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      20                   to   v-lin                  .
           move      18                   to   v-pos                  .
           move      rr-ssu-cod-stt       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-stt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione codice statistico     *
      *    *-----------------------------------------------------------*
       vis-cod-stt-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-ssu-cod-stt-des   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-stt-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Selezione su status commerciale      *
      *    *-----------------------------------------------------------*
       acc-sta-tus-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-sta-tus-025.
      *                  *---------------------------------------------*
      *                  * Preparazione valore di default              *
      *                  *---------------------------------------------*
           if        rr-ssu-sta-tus       =    spaces
                     move  "X        "    to   rr-ssu-sta-tus         .
       acc-sta-tus-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "C"                  to   v-tip                  .
           move      w-exp-sta-tus-lun    to   v-car                  .
           move      w-exp-sta-tus-num    to   v-ldt                  .
           move      "B"                  to   v-edm                  .
           move      21                   to   v-lin                  .
           move      18                   to   v-pos                  .
           move      w-exp-sta-tus-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-ssu-sta-tus       to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-sta-tus-999.
       acc-sta-tus-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-ssu-sta-tus         .
       acc-sta-tus-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-sta-tus-425.
      *                  *---------------------------------------------*
      *                  * Valore a Spaces non ammesso                 *
      *                  *---------------------------------------------*
           if        rr-ssu-sta-tus       =    spaces
                     go to acc-sta-tus-100.
       acc-sta-tus-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sta-tus-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-sta-tus-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-sta-tus-100.
       acc-sta-tus-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Selezione su status commerciale   *
      *    *-----------------------------------------------------------*
       vis-sta-tus-000.
           move      "DS"                 to   v-ope                  .
           move      "C"                  to   v-tip                  .
           move      w-exp-sta-tus-lun    to   v-car                  .
           move      w-exp-sta-tus-num    to   v-ldt                  .
           move      "B"                  to   v-edm                  .
           move      21                   to   v-lin                  .
           move      18                   to   v-pos                  .
           move      w-exp-sta-tus-tbl    to   v-txt                  .
           move      rr-ssu-sta-tus       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sta-tus-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : C.A.P.                               *
      *    *-----------------------------------------------------------*
       acc-cod-cap-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-cap-025.
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore 01..02           *
      *                  *---------------------------------------------*
           move      1                    to   w-acc-cod-cap-inx      .
       acc-cod-cap-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "Massimo 2 CAP - Utilizzare TAB per CAP successivo"
                                          to   v-nt1                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      18                   to   v-lin                  .
      *
           if        w-acc-cod-cap-inx    =    1
                     move  67             to   v-pos
           else if   w-acc-cod-cap-inx    =    2
                     move  74             to   v-pos
           else      move  67             to   v-pos                  .
      *
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "TAB "               to   v-pfk (10)             .
           move      rr-ssu-cod-cap
                    (w-acc-cod-cap-inx)   to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione eventuali note operative          *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cod-cap-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-cap-999.
       acc-cod-cap-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-ssu-cod-cap
                                              (w-acc-cod-cap-inx)     .
       acc-cod-cap-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-cap-405.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tasto funzione   *
      *                  * impostato                                   *
      *                  *---------------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-cod-cap-415
           else if   v-key                =    "DOWN"
                     go to acc-cod-cap-420
           else if   v-key                =    "DO  "
                     go to acc-cod-cap-425
           else if   v-key                =    "TAB "
                     go to acc-cod-cap-430
           else      go to acc-cod-cap-410.
       acc-cod-cap-410.
      *                  *---------------------------------------------*
      *                  * Se tasto funzione Return                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A dipendenze da impostazione            *
      *                      *-----------------------------------------*
           go to     acc-cod-cap-600.
       acc-cod-cap-415.
      *                  *---------------------------------------------*
      *                  * Se tasto funzione Up                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se indice attuale a 1 si va' alle di-   *
      *                      * pendenze da impostazione, altrimenti    *
      *                      * si ricicla su elemento precedente       *
      *                      *-----------------------------------------*
           if        w-acc-cod-cap-inx    =    1
                     go to acc-cod-cap-600.
           subtract  1                    from w-acc-cod-cap-inx      .
           go to     acc-cod-cap-100.
       acc-cod-cap-420.
      *                  *---------------------------------------------*
      *                  * Se tasto funzione Down                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A dipendenze da impostazione            *
      *                      *-----------------------------------------*
           go to     acc-cod-cap-600.
       acc-cod-cap-425.
      *                  *---------------------------------------------*
      *                  * Se tasto funzione Do                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A dipendenze da impostazione            *
      *                      *-----------------------------------------*
           go to     acc-cod-cap-600.
       acc-cod-cap-430.
      *                  *---------------------------------------------*
      *                  * Se tasto funzione Tab                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se indice attuale a 2 si va' alle di-   *
      *                      * pendenze da impostazione, con Down, al- *
      *                      * trimenti si ricicla su elemento prece-  *
      *                      * dente                                   *
      *                      *-----------------------------------------*
           if        w-acc-cod-cap-inx    =    w-acc-cod-cap-max
                     move  "DOWN"         to   v-key
                     go to acc-cod-cap-600.
           add       1                    to   w-acc-cod-cap-inx      .
           go to     acc-cod-cap-100.
       acc-cod-cap-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-cap-625.
      *                  *---------------------------------------------*
      *                  * Compattamento e rivisualizzazione dei 2     *
      *                  * valori                                      *
      *                  *---------------------------------------------*
           move      zero                 to   w-acc-cod-cap-inx      .
           move      zero                 to   w-acc-cod-cap-i02      .
       acc-cod-cap-626.
           add       1                    to   w-acc-cod-cap-inx      .
           if        w-acc-cod-cap-inx    >    w-acc-cod-cap-max
                     go to acc-cod-cap-627.
           if        rr-ssu-cod-cap
                    (w-acc-cod-cap-inx)   =    spaces
                     go to acc-cod-cap-626.
           add       1                    to   w-acc-cod-cap-i02      .
           if        w-acc-cod-cap-i02    =    w-acc-cod-cap-inx
                     go to acc-cod-cap-626.
           move      rr-ssu-cod-cap
                    (w-acc-cod-cap-inx)   to   rr-ssu-cod-cap
                                              (w-acc-cod-cap-i02)     .
           go to     acc-cod-cap-626.
       acc-cod-cap-627.
           add       1                    to   w-acc-cod-cap-i02      .
           if        w-acc-cod-cap-i02    >    w-acc-cod-cap-max
                     go to acc-cod-cap-628.
           move      spaces               to   rr-ssu-cod-cap
                                              (w-acc-cod-cap-i02)     .
           go to     acc-cod-cap-627.
       acc-cod-cap-628.
           perform   vis-cod-cap-000      thru vis-cod-cap-999        .
       acc-cod-cap-650.
      *                  *---------------------------------------------*
      *                  * Fine dipendenze dall'impostazione           *
      *                  *---------------------------------------------*
           go to     acc-cod-cap-800.
       acc-cod-cap-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-cap-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-cap-100.
       acc-cod-cap-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : C.A.P.                            *
      *    *-----------------------------------------------------------*
       vis-cod-cap-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione contatore 01..02               *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-cod-cap-inx      .
       vis-cod-cap-100.
      *              *-------------------------------------------------*
      *              * Incremento contatore 01..02                     *
      *              *-------------------------------------------------*
           add       1                    to   w-acc-cod-cap-inx      .
      *              *-------------------------------------------------*
      *              * Test se oltre il max                            *
      *              *-------------------------------------------------*
           if        w-acc-cod-cap-inx    >    w-acc-cod-cap-max
                     go to vis-cod-cap-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      18                   to   v-lin                  .
      *
           if        w-acc-cod-cap-inx    =    1
                     move  67             to   v-pos
           else if   w-acc-cod-cap-inx    =    2
                     move  74             to   v-pos
           else      move  67             to   v-pos                  .
      *
           move      rr-ssu-cod-cap
                    (w-acc-cod-cap-inx)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Riciclo su elemento successivo                  *
      *              *-------------------------------------------------*
           go to     vis-cod-cap-100.
       vis-cod-cap-999.
           exit.

      *    *===========================================================*
      *    * Controllo su tasto Do in parametri di selezione           *
      *    *-----------------------------------------------------------*
       tdo-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ric-flg      .
       tdo-ric-sel-010.
      *              *-------------------------------------------------*
      *              * Controllo su tipo ordinamento stampa            *
      *              *-------------------------------------------------*
           if        rr-tip-ors           =    01 or
                     rr-tip-ors           =    03 or
                     rr-tip-ors           =    04 or
                     rr-tip-ors           =    05 or
                     rr-tip-ors           =    07 or
                     rr-tip-ors           =    11 or
                     rr-tip-ors           =    12 or
                     rr-tip-ors           =    13 or
                     rr-tip-ors           =    14 or
                     rr-tip-ors           =    21 or
                     rr-tip-ors           =    22
                     go to tdo-ric-sel-020.
           move      "Tipo ordinamento stampa errato !                  
      -              "                "   to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-020.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo ordinamento     *
      *              *-------------------------------------------------*
           if        rr-tip-ors           =    01
                     go to tdo-ric-sel-100
           else if   rr-tip-ors           =    03
                     go to tdo-ric-sel-300
           else if   rr-tip-ors           =    04
                     go to tdo-ric-sel-400
           else if   rr-tip-ors           =    05
                     go to tdo-ric-sel-500
           else if   rr-tip-ors           =    07
                     go to tdo-ric-sel-a00
           else if   rr-tip-ors           =    11
                     go to tdo-ric-sel-610
           else if   rr-tip-ors           =    12
                     go to tdo-ric-sel-620
           else if   rr-tip-ors           =    13
                     go to tdo-ric-sel-630
           else if   rr-tip-ors           =    14
                     go to tdo-ric-sel-640
           else if   rr-tip-ors           =    21 or
                     rr-tip-ors           =    22
                     go to tdo-ric-sel-710
           else      go to tdo-ric-sel-800.
       tdo-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Se ordinamento per fornitore                    *
      *              *-------------------------------------------------*
           perform   tdo-ric-sel-fnt-000  thru tdo-ric-sel-fnt-999    .
      *                  *---------------------------------------------*
      *                  * Test sul flag di uscita                     *
      *                  *---------------------------------------------*
           if        w-cnt-tdo-ric-flg    not  = spaces
                     go to tdo-ric-sel-999.
      *                  *---------------------------------------------*
      *                  * Ad ulteriori regolarizzazioni               *
      *                  *---------------------------------------------*
           go to     tdo-ric-sel-800.
       tdo-ric-sel-300.
      *              *-------------------------------------------------*
      *              * Se ordinamento per zona                         *
      *              *-------------------------------------------------*
           perform   tdo-ric-sel-zon-000  thru tdo-ric-sel-zon-999    .
      *                  *---------------------------------------------*
      *                  * Test sul flag di uscita                     *
      *                  *---------------------------------------------*
           if        w-cnt-tdo-ric-flg    not  = spaces
                     go to tdo-ric-sel-999.
      *                  *---------------------------------------------*
      *                  * Ad ulteriori regolarizzazioni               *
      *                  *---------------------------------------------*
           go to     tdo-ric-sel-800.
       tdo-ric-sel-400.
      *              *-------------------------------------------------*
      *              * Se ordinamento per categoria                    *
      *              *-------------------------------------------------*
           perform   tdo-ric-sel-cat-000  thru tdo-ric-sel-cat-999    .
      *                  *---------------------------------------------*
      *                  * Test sul flag di uscita                     *
      *                  *---------------------------------------------*
           if        w-cnt-tdo-ric-flg    not  = spaces
                     go to tdo-ric-sel-999.
      *                  *---------------------------------------------*
      *                  * Ad ulteriori regolarizzazioni               *
      *                  *---------------------------------------------*
           go to     tdo-ric-sel-800.
       tdo-ric-sel-500.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codice statistico            *
      *              *-------------------------------------------------*
           perform   tdo-ric-sel-stt-000  thru tdo-ric-sel-stt-999    .
      *                  *---------------------------------------------*
      *                  * Test sul flag di uscita                     *
      *                  *---------------------------------------------*
           if        w-cnt-tdo-ric-flg    not  = spaces
                     go to tdo-ric-sel-999.
      *                  *---------------------------------------------*
      *                  * Ad ulteriori regolarizzazioni               *
      *                  *---------------------------------------------*
           go to     tdo-ric-sel-800.
       tdo-ric-sel-a00.
      *              *-------------------------------------------------*
      *              * Se ordinamento per forma di pagamento           *
      *              *-------------------------------------------------*
           perform   tdo-ric-sel-fop-000  thru tdo-ric-sel-fop-999    .
      *                  *---------------------------------------------*
      *                  * Test sul flag di uscita                     *
      *                  *---------------------------------------------*
           if        w-cnt-tdo-ric-flg    not  = spaces
                     go to tdo-ric-sel-999.
      *                  *---------------------------------------------*
      *                  * Ad ulteriori regolarizzazioni               *
      *                  *---------------------------------------------*
           go to     tdo-ric-sel-800.
       tdo-ric-sel-610.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codice nazione               *
      *              *-------------------------------------------------*
           perform   tdo-ric-sel-naz-000  thru tdo-ric-sel-naz-999    .
      *                  *---------------------------------------------*
      *                  * Test sul flag di uscita                     *
      *                  *---------------------------------------------*
           if        w-cnt-tdo-ric-flg    not  = spaces
                     go to tdo-ric-sel-999.
      *                  *---------------------------------------------*
      *                  * Ad ulteriori regolarizzazioni               *
      *                  *---------------------------------------------*
           go to     tdo-ric-sel-800.
       tdo-ric-sel-620.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codice regione               *
      *              *-------------------------------------------------*
           perform   tdo-ric-sel-rgn-000  thru tdo-ric-sel-rgn-999    .
      *                  *---------------------------------------------*
      *                  * Test sul flag di uscita                     *
      *                  *---------------------------------------------*
           if        w-cnt-tdo-ric-flg    not  = spaces
                     go to tdo-ric-sel-999.
      *                  *---------------------------------------------*
      *                  * Ad ulteriori regolarizzazioni               *
      *                  *---------------------------------------------*
           go to     tdo-ric-sel-800.
       tdo-ric-sel-630.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codice provincia             *
      *              *-------------------------------------------------*
           perform   tdo-ric-sel-prv-000  thru tdo-ric-sel-prv-999    .
      *                  *---------------------------------------------*
      *                  * Test sul flag di uscita                     *
      *                  *---------------------------------------------*
           if        w-cnt-tdo-ric-flg    not  = spaces
                     go to tdo-ric-sel-999.
      *                  *---------------------------------------------*
      *                  * Ad ulteriori regolarizzazioni               *
      *                  *---------------------------------------------*
           go to     tdo-ric-sel-800.
       tdo-ric-sel-640.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codice comune                *
      *              *-------------------------------------------------*
           perform   tdo-ric-sel-cmn-000  thru tdo-ric-sel-cmn-999    .
      *                  *---------------------------------------------*
      *                  * Test sul flag di uscita                     *
      *                  *---------------------------------------------*
           if        w-cnt-tdo-ric-flg    not  = spaces
                     go to tdo-ric-sel-999.
      *                  *---------------------------------------------*
      *                  * Ad ulteriori regolarizzazioni               *
      *                  *---------------------------------------------*
           go to     tdo-ric-sel-800.
       tdo-ric-sel-710.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici avviamento postale    *
      *              *-------------------------------------------------*
           perform   tdo-ric-sel-cap-000  thru tdo-ric-sel-cap-999    .
      *                  *---------------------------------------------*
      *                  * Test sul flag di uscita                     *
      *                  *---------------------------------------------*
           if        w-cnt-tdo-ric-flg    not  = spaces
                     go to tdo-ric-sel-999.
      *                  *---------------------------------------------*
      *                  * Ad ulteriori regolarizzazioni               *
      *                  *---------------------------------------------*
           go to     tdo-ric-sel-800.
       tdo-ric-sel-800.
      *              *-------------------------------------------------*
      *              * Controllo su tipo fornitori a parita' di ana-   *
      *              * grafica di ordinamento                          *
      *              *-------------------------------------------------*
           if        rr-tip-ors           =    01
                     go to tdo-ric-sel-820.
           if        rr-tip-ocp           =    01 or
                     rr-tip-ocp           =    02 or
                     rr-tip-ocp           =    03
                     go to tdo-ric-sel-820.
           move      "Tipo ordinamento fornitori errato !               
      -              "                "   to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-820.
      *              *-------------------------------------------------*
      *              * Controllo su tipo di stampa                     *
      *              *-------------------------------------------------*
           if        rr-tip-stp           =    01 or
                     rr-tip-stp           =    11 or
                     rr-tip-stp           =    21 or
                     rr-tip-stp           =    31 or
                     rr-tip-stp           =    32 or
                     rr-tip-stp           =    51
                     go to tdo-ric-sel-840.
           move      "Tipo di stampa errato !                           
      -              "                "   to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-840.
      *              *-------------------------------------------------*
      *              * Controllo su si/no salto pagina a rottura       *
      *              *-------------------------------------------------*
           if        rr-tip-ors           =    01
                     go to tdo-ric-sel-860.
           if        rr-tip-stp           =    21 or
                     rr-tip-stp           =    31 or
                     rr-tip-stp           =    32 or
                     rr-tip-stp           =    51
                     go to tdo-ric-sel-860.
           if        rr-sns-prt           =    01 or
                     rr-sns-prt           =    02
                     go to tdo-ric-sel-860.
           move      "Si/no salto pagina errato !                       
      -              "                "   to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-860.
      *              *-------------------------------------------------*
      *              * Controllo su tipo di etichette                  *
      *              *-------------------------------------------------*
           if        rr-tip-stp           =    01 or
                     rr-tip-stp           =    11 or
                     rr-tip-stp           =    21 or
                     rr-tip-stp           =    51
                     go to tdo-ric-sel-880.
           if        rr-tip-eti           =    01 or
                     rr-tip-eti           =    51
                     go to tdo-ric-sel-880.
           move      "Tipo di etichette errato !                        
      -              "                "   to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-880.
      *              *-------------------------------------------------*
      *              * Uscita per controlli tutti superati             *
      *              *-------------------------------------------------*
           go to     tdo-ric-sel-999.
       tdo-ric-sel-900.
      *              *-------------------------------------------------*
      *              * Emissione messaggio di errore e set del flag di *
      *              * uscita ad errore                                *
      *              *-------------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di errore in uscita                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-ric-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     tdo-ric-sel-999.
       tdo-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Controllo su tasto Do in parametri di selezione           *
      *    *                                                           *
      *    * Fornitore                                                 *
      *    *-----------------------------------------------------------*
       tdo-ric-sel-fnt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ric-flg      .
      *              *-------------------------------------------------*
      *              * Controllo su tipo ordinamento anagrafica forni- *
      *              * tori                                            *
      *              *-------------------------------------------------*
           if        rr-ord-ana-fnt       =    01 or
                     rr-ord-ana-fnt       =    02 or
                     rr-ord-ana-fnt       =    03
                     go to tdo-ric-sel-fnt-100.
           move      "Tipo ordinamento Fornitori errato !               
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-fnt-100.
      *              *-------------------------------------------------*
      *              * Controllo su ragione sociale iniziale e finale  *
      *              *-------------------------------------------------*
           if        rr-sel-fnt-rsf       =    spaces
                     go to tdo-ric-sel-fnt-200.
           if        rr-sel-fnt-rsf       not  < rr-sel-fnt-rsi
                     go to tdo-ric-sel-fnt-200.
           move      "Ragione sociale Fornitore finale inferiore a quell
      -              "a iniziale !    "   to   w-err-box-err-msg      .
           go to     tdo-ric-sel-fnt-900.
       tdo-ric-sel-fnt-200.
      *              *-------------------------------------------------*
      *              * Controllo su codice minimo e massimo            *
      *              *-------------------------------------------------*
           if        rr-sel-fnt-cof       =    zero
                     go to tdo-ric-sel-fnt-400.
           if        rr-sel-fnt-cof       not  < rr-sel-fnt-coi
                     go to tdo-ric-sel-fnt-400.
           move      "Codice Fornitore finale inferiore a quello inizial
      -              "e !             "   to   w-err-box-err-msg      .
           go to     tdo-ric-sel-fnt-900.
       tdo-ric-sel-fnt-400.
      *              *-------------------------------------------------*
      *              * Controllo su mnemonico minimo e massimo         *
      *              *-------------------------------------------------*
           if        rr-sel-fnt-mnf       =    spaces
                     go to tdo-ric-sel-800.
           if        rr-sel-fnt-mnf       not  < rr-sel-fnt-mni
                     go to tdo-ric-sel-800.
           move      "Mnemonico Fornitore finale inferiore a quello iniz
      -              "iale !          "   to   w-err-box-err-msg      .
           go to     tdo-ric-sel-fnt-900.
       tdo-ric-sel-fnt-800.
      *              *-------------------------------------------------*
      *              * Uscita per controlli tutti superati             *
      *              *-------------------------------------------------*
           go to     tdo-ric-sel-fnt-999.
       tdo-ric-sel-fnt-900.
      *              *-------------------------------------------------*
      *              * Emissione messaggio di errore e set del flag di *
      *              * uscita ad errore                                *
      *              *-------------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di errore in uscita                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-ric-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     tdo-ric-sel-fnt-999.
       tdo-ric-sel-fnt-999.
           exit.

      *    *===========================================================*
      *    * Controllo su tasto Do in parametri di selezione           *
      *    *                                                           *
      *    * Zona                                                      *
      *    *-----------------------------------------------------------*
       tdo-ric-sel-zon-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ric-flg      .
      *              *-------------------------------------------------*
      *              * Controllo su tipo ordinamento anagrafica zone   *
      *              *-------------------------------------------------*
           if        rr-ord-ana-zon       =    01 or
                     rr-ord-ana-zon       =    02 or
                     rr-ord-ana-zon       =    03
                     go to tdo-ric-sel-zon-100.
           move      "Tipo ordinamento Zone errato !                    
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-zon-100.
      *              *-------------------------------------------------*
      *              * Controllo su descrizione iniziale e finale      *
      *              *-------------------------------------------------*
           if        rr-sel-zon-def       =    spaces
                     go to tdo-ric-sel-zon-200.
           if        rr-sel-zon-def       not  < rr-sel-zon-dei
                     go to tdo-ric-sel-zon-200.
           move      "Descrizione Zona finale inferiore a quella inizial
      -              "e !             "   to   w-err-box-err-msg      .
           go to     tdo-ric-sel-zon-900.
       tdo-ric-sel-zon-200.
      *              *-------------------------------------------------*
      *              * Controllo su codice minimo e massimo            *
      *              *-------------------------------------------------*
           if        rr-sel-zon-cof       =    zero
                     go to tdo-ric-sel-zon-400.
           if        rr-sel-zon-cof       not  < rr-sel-zon-coi
                     go to tdo-ric-sel-zon-400.
           move      "Codice Zona finale inferiore a quello iniziale !  
      -              "                "   to   w-err-box-err-msg      .
           go to     tdo-ric-sel-zon-900.
       tdo-ric-sel-zon-400.
      *              *-------------------------------------------------*
      *              * Controllo su mnemonico minimo e massimo         *
      *              *-------------------------------------------------*
           if        rr-sel-zon-mnf       =    spaces
                     go to tdo-ric-sel-800.
           if        rr-sel-zon-mnf       not  < rr-sel-zon-mni
                     go to tdo-ric-sel-800.
           move      "Mnemonico Zona finale inferiore a quello iniziale 
      -              "!               "   to   w-err-box-err-msg      .
           go to     tdo-ric-sel-zon-900.
       tdo-ric-sel-zon-800.
      *              *-------------------------------------------------*
      *              * Uscita per controlli tutti superati             *
      *              *-------------------------------------------------*
           go to     tdo-ric-sel-zon-999.
       tdo-ric-sel-zon-900.
      *              *-------------------------------------------------*
      *              * Emissione messaggio di errore e set del flag di *
      *              * uscita ad errore                                *
      *              *-------------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di errore in uscita                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-ric-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     tdo-ric-sel-zon-999.
       tdo-ric-sel-zon-999.
           exit.

      *    *===========================================================*
      *    * Controllo su tasto Do in parametri di selezione           *
      *    *                                                           *
      *    * Zona                                                      *
      *    *-----------------------------------------------------------*
       tdo-ric-sel-cat-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ric-flg      .
      *              *-------------------------------------------------*
      *              * Controllo su tipo ordinamento anagrafica cate-  *
      *              * gorie                                           *
      *              *-------------------------------------------------*
           if        rr-ord-ana-cat       =    01 or
                     rr-ord-ana-cat       =    02 or
                     rr-ord-ana-cat       =    03
                     go to tdo-ric-sel-cat-100.
           move      "Tipo ordinamento Categorie errato !               
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-cat-100.
      *              *-------------------------------------------------*
      *              * Controllo su descrizione iniziale e finale      *
      *              *-------------------------------------------------*
           if        rr-sel-cat-def       =    spaces
                     go to tdo-ric-sel-cat-200.
           if        rr-sel-cat-def       not  < rr-sel-cat-dei
                     go to tdo-ric-sel-cat-200.
           move      "Descrizione Categoria finale inferiore a quella in
      -              "iziale !        "   to   w-err-box-err-msg      .
           go to     tdo-ric-sel-cat-900.
       tdo-ric-sel-cat-200.
      *              *-------------------------------------------------*
      *              * Controllo su codice minimo e massimo            *
      *              *-------------------------------------------------*
           if        rr-sel-cat-cof       =    zero
                     go to tdo-ric-sel-cat-400.
           if        rr-sel-cat-cof       not  < rr-sel-cat-coi
                     go to tdo-ric-sel-cat-400.
           move      "Codice Categoria finale inferiore a quello inizial
      -              "e !             "   to   w-err-box-err-msg      .
           go to     tdo-ric-sel-cat-900.
       tdo-ric-sel-cat-400.
      *              *-------------------------------------------------*
      *              * Controllo su mnemonico minimo e massimo         *
      *              *-------------------------------------------------*
           if        rr-sel-cat-mnf       =    spaces
                     go to tdo-ric-sel-800.
           if        rr-sel-cat-mnf       not  < rr-sel-cat-mni
                     go to tdo-ric-sel-800.
           move      "Mnemonico Categoria finale inferiore a quello iniz
      -              "iale !          "   to   w-err-box-err-msg      .
           go to     tdo-ric-sel-cat-900.
       tdo-ric-sel-cat-800.
      *              *-------------------------------------------------*
      *              * Uscita per controlli tutti superati             *
      *              *-------------------------------------------------*
           go to     tdo-ric-sel-cat-999.
       tdo-ric-sel-cat-900.
      *              *-------------------------------------------------*
      *              * Emissione messaggio di errore e set del flag di *
      *              * uscita ad errore                                *
      *              *-------------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di errore in uscita                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-ric-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     tdo-ric-sel-cat-999.
       tdo-ric-sel-cat-999.
           exit.

      *    *===========================================================*
      *    * Controllo su tasto Do in parametri di selezione           *
      *    *                                                           *
      *    * Codici statistici                                         *
      *    *-----------------------------------------------------------*
       tdo-ric-sel-stt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ric-flg      .
      *              *-------------------------------------------------*
      *              * Controllo su tipo ordinamento anagrafica codi-  *
      *              * ci statistici                                   *
      *              *-------------------------------------------------*
           if        rr-ord-ana-stt       =    01 or
                     rr-ord-ana-stt       =    02 or
                     rr-ord-ana-stt       =    03
                     go to tdo-ric-sel-stt-100.
           move      "Tipo ordinamento Codici statistici errato !       
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-stt-100.
      *              *-------------------------------------------------*
      *              * Controllo su descrizione iniziale e finale      *
      *              *-------------------------------------------------*
           if        rr-sel-stt-def       =    spaces
                     go to tdo-ric-sel-stt-200.
           if        rr-sel-stt-def       not  < rr-sel-stt-dei
                     go to tdo-ric-sel-stt-200.
           move      "Descrizione Codice statistico finale inferiore a q
      -              "uella iniziale !"   to   w-err-box-err-msg      .
           go to     tdo-ric-sel-stt-900.
       tdo-ric-sel-stt-200.
      *              *-------------------------------------------------*
      *              * Controllo su codice minimo e massimo            *
      *              *-------------------------------------------------*
           if        rr-sel-stt-cof       =    zero
                     go to tdo-ric-sel-stt-400.
           if        rr-sel-stt-cof       not  < rr-sel-stt-coi
                     go to tdo-ric-sel-stt-400.
           move      "Codice statistico finale inferiore a quello inizia
      -              "le !            "   to   w-err-box-err-msg      .
           go to     tdo-ric-sel-stt-900.
       tdo-ric-sel-stt-400.
      *              *-------------------------------------------------*
      *              * Controllo su mnemonico minimo e massimo         *
      *              *-------------------------------------------------*
           if        rr-sel-stt-mnf       =    spaces
                     go to tdo-ric-sel-800.
           if        rr-sel-stt-mnf       not  < rr-sel-stt-mni
                     go to tdo-ric-sel-800.
           move      "Mnemonico Codice statistico finale inferiore a que
      -              "llo iniziale !  "   to   w-err-box-err-msg      .
           go to     tdo-ric-sel-stt-900.
       tdo-ric-sel-stt-800.
      *              *-------------------------------------------------*
      *              * Uscita per controlli tutti superati             *
      *              *-------------------------------------------------*
           go to     tdo-ric-sel-stt-999.
       tdo-ric-sel-stt-900.
      *              *-------------------------------------------------*
      *              * Emissione messaggio di errore e set del flag di *
      *              * uscita ad errore                                *
      *              *-------------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di errore in uscita                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-ric-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     tdo-ric-sel-stt-999.
       tdo-ric-sel-stt-999.
           exit.

      *    *===========================================================*
      *    * Controllo su tasto Do in parametri di selezione           *
      *    *                                                           *
      *    * Forme di pagamento                                        *
      *    *-----------------------------------------------------------*
       tdo-ric-sel-fop-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ric-flg      .
      *              *-------------------------------------------------*
      *              * Controllo su tipo ordinamento anagrafica forme  *
      *              * di pagamento                                    *
      *              *-------------------------------------------------*
           if        rr-ord-ana-fop       =    01 or
                     rr-ord-ana-fop       =    02 or
                     rr-ord-ana-fop       =    03
                     go to tdo-ric-sel-fop-100.
           move      "Tipo ordinamento Forme di pagamento errato !      
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-fop-100.
      *              *-------------------------------------------------*
      *              * Controllo su descrizione iniziale e finale      *
      *              *-------------------------------------------------*
           if        rr-sel-fop-def       =    spaces
                     go to tdo-ric-sel-fop-200.
           if        rr-sel-fop-def       not  < rr-sel-fop-dei
                     go to tdo-ric-sel-fop-200.
           move      "Descrizione Forma di pagamento finale inferiore a 
      -              "quella iniziale "   to   w-err-box-err-msg      .
           go to     tdo-ric-sel-fop-900.
       tdo-ric-sel-fop-200.
      *              *-------------------------------------------------*
      *              * Controllo su codice minimo e massimo            *
      *              *-------------------------------------------------*
           if        rr-sel-fop-cof       =    zero
                     go to tdo-ric-sel-fop-400.
           if        rr-sel-fop-cof       not  < rr-sel-fop-coi
                     go to tdo-ric-sel-fop-400.
           move      "Forma di pagamento finale inferiore a quella inizi
      -              "ale !           "   to   w-err-box-err-msg      .
           go to     tdo-ric-sel-fop-900.
       tdo-ric-sel-fop-400.
      *              *-------------------------------------------------*
      *              * Controllo su mnemonico minimo e massimo         *
      *              *-------------------------------------------------*
           if        rr-sel-fop-mnf       =    spaces
                     go to tdo-ric-sel-800.
           if        rr-sel-fop-mnf       not  < rr-sel-fop-mni
                     go to tdo-ric-sel-800.
           move      "Mnemonico Forma di pagamento finale inferiore a qu
      -              "ello iniziale !"   to   w-err-box-err-msg      .
           go to     tdo-ric-sel-fop-900.
       tdo-ric-sel-fop-800.
      *              *-------------------------------------------------*
      *              * Uscita per controlli tutti superati             *
      *              *-------------------------------------------------*
           go to     tdo-ric-sel-fop-999.
       tdo-ric-sel-fop-900.
      *              *-------------------------------------------------*
      *              * Emissione messaggio di errore e set del flag di *
      *              * uscita ad errore                                *
      *              *-------------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di errore in uscita                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-ric-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     tdo-ric-sel-fop-999.
       tdo-ric-sel-fop-999.
           exit.

      *    *===========================================================*
      *    * Controllo su tasto Do in parametri di selezione           *
      *    *                                                           *
      *    * Codici nazione                                            *
      *    *-----------------------------------------------------------*
       tdo-ric-sel-naz-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ric-flg      .
      *              *-------------------------------------------------*
      *              * Controllo su tipo ordinamento anagrafica codi-  *
      *              * ci nazione                                      *
      *              *-------------------------------------------------*
           if        rr-ord-ana-naz       =    01 or
                     rr-ord-ana-naz       =    02
                     go to tdo-ric-sel-naz-100.
           move      "Tipo ordinamento Codici nazione errato !          
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-naz-100.
      *              *-------------------------------------------------*
      *              * Controllo su descrizione iniziale e finale      *
      *              *-------------------------------------------------*
           if        rr-sel-naz-def       =    spaces
                     go to tdo-ric-sel-naz-200.
           if        rr-sel-naz-def       not  < rr-sel-naz-dei
                     go to tdo-ric-sel-naz-200.
           move      "Descrizione Codice nazione finale inferiore a quel
      -              "la iniziale !   "   to   w-err-box-err-msg      .
           go to     tdo-ric-sel-naz-900.
       tdo-ric-sel-naz-200.
      *              *-------------------------------------------------*
      *              * Controllo su codice minimo e massimo            *
      *              *-------------------------------------------------*
           if        rr-sel-naz-cof       =    spaces
                     go to tdo-ric-sel-naz-800.
           if        rr-sel-naz-cof       not  < rr-sel-naz-coi
                     go to tdo-ric-sel-naz-800.
           move      "Codice nazione finale inferiore a quello iniziale 
      -              "!               "   to   w-err-box-err-msg      .
           go to     tdo-ric-sel-naz-900.
       tdo-ric-sel-naz-800.
      *              *-------------------------------------------------*
      *              * Uscita per controlli tutti superati             *
      *              *-------------------------------------------------*
           go to     tdo-ric-sel-naz-999.
       tdo-ric-sel-naz-900.
      *              *-------------------------------------------------*
      *              * Emissione messaggio di errore e set del flag di *
      *              * uscita ad errore                                *
      *              *-------------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di errore in uscita                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-ric-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     tdo-ric-sel-naz-999.
       tdo-ric-sel-naz-999.
           exit.

      *    *===========================================================*
      *    * Controllo su tasto Do in parametri di selezione           *
      *    *                                                           *
      *    * Codici regione                                            *
      *    *-----------------------------------------------------------*
       tdo-ric-sel-rgn-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ric-flg      .
      *              *-------------------------------------------------*
      *              * Controllo su tipo ordinamento anagrafica codi-  *
      *              * ci regione                                      *
      *              *-------------------------------------------------*
           if        rr-ord-ana-rgn       =    01 or
                     rr-ord-ana-rgn       =    02
                     go to tdo-ric-sel-rgn-100.
           move      "Tipo ordinamento Codici regione errato !          
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-rgn-100.
      *              *-------------------------------------------------*
      *              * Controllo su descrizione iniziale e finale      *
      *              *-------------------------------------------------*
           if        rr-sel-rgn-def       =    spaces
                     go to tdo-ric-sel-rgn-200.
           if        rr-sel-rgn-def       not  < rr-sel-rgn-dei
                     go to tdo-ric-sel-rgn-200.
           move      "Descrizione Codice regione finale inferiore a quel
      -              "la iniziale !   "   to   w-err-box-err-msg      .
           go to     tdo-ric-sel-rgn-900.
       tdo-ric-sel-rgn-200.
      *              *-------------------------------------------------*
      *              * Controllo su codice minimo e massimo            *
      *              *-------------------------------------------------*
           if        rr-sel-rgn-cof       =    spaces
                     go to tdo-ric-sel-rgn-800.
           if        rr-sel-rgn-cof       not  < rr-sel-rgn-coi
                     go to tdo-ric-sel-rgn-800.
           move      "Codice regione finale inferiore a quello iniziale 
      -              "!               "   to   w-err-box-err-msg      .
           go to     tdo-ric-sel-rgn-900.
       tdo-ric-sel-rgn-800.
      *              *-------------------------------------------------*
      *              * Uscita per controlli tutti superati             *
      *              *-------------------------------------------------*
           go to     tdo-ric-sel-rgn-999.
       tdo-ric-sel-rgn-900.
      *              *-------------------------------------------------*
      *              * Emissione messaggio di errore e set del flag di *
      *              * uscita ad errore                                *
      *              *-------------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di errore in uscita                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-ric-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     tdo-ric-sel-rgn-999.
       tdo-ric-sel-rgn-999.
           exit.

      *    *===========================================================*
      *    * Controllo su tasto Do in parametri di selezione           *
      *    *                                                           *
      *    * Codici provincia                                          *
      *    *-----------------------------------------------------------*
       tdo-ric-sel-prv-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ric-flg      .
      *              *-------------------------------------------------*
      *              * Controllo su tipo ordinamento anagrafica codi-  *
      *              * ci provincia                                    *
      *              *-------------------------------------------------*
           if        rr-ord-ana-prv       =    01 or
                     rr-ord-ana-prv       =    02
                     go to tdo-ric-sel-prv-100.
           move      "Tipo ordinamento Codici provincia errato !        
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-prv-100.
      *              *-------------------------------------------------*
      *              * Controllo su descrizione iniziale e finale      *
      *              *-------------------------------------------------*
           if        rr-sel-prv-def       =    spaces
                     go to tdo-ric-sel-prv-200.
           if        rr-sel-prv-def       not  < rr-sel-prv-dei
                     go to tdo-ric-sel-prv-200.
           move      "Descrizione Codice provincia finale inferiore a qu
      -              "ella iniziale ! "   to   w-err-box-err-msg      .
           go to     tdo-ric-sel-prv-900.
       tdo-ric-sel-prv-200.
      *              *-------------------------------------------------*
      *              * Controllo su codice minimo e massimo            *
      *              *-------------------------------------------------*
           if        rr-sel-prv-cof       =    spaces
                     go to tdo-ric-sel-prv-800.
           if        rr-sel-prv-cof       not  < rr-sel-prv-coi
                     go to tdo-ric-sel-prv-800.
           move      "Codice provincia finale inferiore a quello inizial
      -              "e !             "   to   w-err-box-err-msg      .
           go to     tdo-ric-sel-prv-900.
       tdo-ric-sel-prv-800.
      *              *-------------------------------------------------*
      *              * Uscita per controlli tutti superati             *
      *              *-------------------------------------------------*
           go to     tdo-ric-sel-prv-999.
       tdo-ric-sel-prv-900.
      *              *-------------------------------------------------*
      *              * Emissione messaggio di errore e set del flag di *
      *              * uscita ad errore                                *
      *              *-------------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di errore in uscita                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-ric-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     tdo-ric-sel-prv-999.
       tdo-ric-sel-prv-999.
           exit.

      *    *===========================================================*
      *    * Controllo su tasto Do in parametri di selezione           *
      *    *                                                           *
      *    * Codici comune                                             *
      *    *-----------------------------------------------------------*
       tdo-ric-sel-cmn-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ric-flg      .
      *              *-------------------------------------------------*
      *              * Controllo su tipo ordinamento anagrafica codi-  *
      *              * ci comune                                       *
      *              *-------------------------------------------------*
           if        rr-ord-ana-cmn       =    01 or
                     rr-ord-ana-cmn       =    02
                     go to tdo-ric-sel-cmn-100.
           move      "Tipo ordinamento Codici provincia errato !        
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-cmn-100.
      *              *-------------------------------------------------*
      *              * Controllo su descrizione iniziale e finale      *
      *              *-------------------------------------------------*
           if        rr-sel-cmn-def       =    spaces
                     go to tdo-ric-sel-cmn-200.
           if        rr-sel-cmn-def       not  < rr-sel-cmn-dei
                     go to tdo-ric-sel-cmn-200.
           move      "Descrizione Codice comune finale inferiore a quell
      -              "a iniziale    ! "   to   w-err-box-err-msg      .
           go to     tdo-ric-sel-cmn-900.
       tdo-ric-sel-cmn-200.
      *              *-------------------------------------------------*
      *              * Controllo su C.a.p. minimo e massimo            *
      *              *-------------------------------------------------*
           if        rr-sel-cmn-cpf       =    spaces
                     go to tdo-ric-sel-cmn-800.
           if        rr-sel-cmn-cpf       not  < rr-sel-cmn-cpi
                     go to tdo-ric-sel-cmn-800.
           move      "C.a.p. del comune iniziale inferiore a quello del c
      -              "omune finale !  "   to   w-err-box-err-msg      .
           go to     tdo-ric-sel-cmn-900.
       tdo-ric-sel-cmn-800.
      *              *-------------------------------------------------*
      *              * Uscita per controlli tutti superati             *
      *              *-------------------------------------------------*
           go to     tdo-ric-sel-cmn-999.
       tdo-ric-sel-cmn-900.
      *              *-------------------------------------------------*
      *              * Emissione messaggio di errore e set del flag di *
      *              * uscita ad errore                                *
      *              *-------------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di errore in uscita                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-ric-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     tdo-ric-sel-cmn-999.
       tdo-ric-sel-cmn-999.
           exit.

      *    *===========================================================*
      *    * Controllo su tasto Do in parametri di selezione           *
      *    *                                                           *
      *    * Codici di avviamento postale                              *
      *    *-----------------------------------------------------------*
       tdo-ric-sel-cap-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ric-flg      .
       tdo-ric-sel-cap-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione dei parametri di selezione               *
      *    *-----------------------------------------------------------*
       reg-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo ordinamento     *
      *              *-------------------------------------------------*
           if        rr-tip-ors           =    01
                     go to reg-ric-sel-100
           else if   rr-tip-ors           =    03
                     go to reg-ric-sel-300
           else if   rr-tip-ors           =    04
                     go to reg-ric-sel-400
           else if   rr-tip-ors           =    05
                     go to reg-ric-sel-500
           else if   rr-tip-ors           =    07
                     go to reg-ric-sel-a00
           else if   rr-tip-ors           =    11
                     go to reg-ric-sel-610
           else if   rr-tip-ors           =    12
                     go to reg-ric-sel-620
           else if   rr-tip-ors           =    13
                     go to reg-ric-sel-630
           else if   rr-tip-ors           =    14
                     go to reg-ric-sel-640
           else if   rr-tip-ors           =    21 or
                     rr-tip-ors           =    22
                     go to reg-ric-sel-710
           else      go to reg-ric-sel-800.
       reg-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Se ordinamento per fornitore                    *
      *              *-------------------------------------------------*
           perform   reg-ric-sel-fnt-000  thru reg-ric-sel-fnt-999    .
      *                  *---------------------------------------------*
      *                  * Ad ulteriori regolarizzazioni               *
      *                  *---------------------------------------------*
           go to     reg-ric-sel-800.
       reg-ric-sel-300.
      *              *-------------------------------------------------*
      *              * Se ordinamento per zona                         *
      *              *-------------------------------------------------*
           perform   reg-ric-sel-zon-000  thru reg-ric-sel-zon-999    .
      *                  *---------------------------------------------*
      *                  * Ad ulteriori regolarizzazioni               *
      *                  *---------------------------------------------*
           go to     reg-ric-sel-800.
       reg-ric-sel-400.
      *              *-------------------------------------------------*
      *              * Se ordinamento per categoria                    *
      *              *-------------------------------------------------*
           perform   reg-ric-sel-cat-000  thru reg-ric-sel-cat-999    .
      *                  *---------------------------------------------*
      *                  * Ad ulteriori regolarizzazioni               *
      *                  *---------------------------------------------*
           go to     reg-ric-sel-800.
       reg-ric-sel-500.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codice statistico            *
      *              *-------------------------------------------------*
           perform   reg-ric-sel-stt-000  thru reg-ric-sel-stt-999    .
      *                  *---------------------------------------------*
      *                  * Ad ulteriori regolarizzazioni               *
      *                  *---------------------------------------------*
           go to     reg-ric-sel-800.
       reg-ric-sel-a00.
      *              *-------------------------------------------------*
      *              * Se ordinamento per forma di pagamento           *
      *              *-------------------------------------------------*
           perform   reg-ric-sel-fop-000  thru reg-ric-sel-fop-999    .
      *                  *---------------------------------------------*
      *                  * Ad ulteriori regolarizzazioni               *
      *                  *---------------------------------------------*
           go to     reg-ric-sel-800.
       reg-ric-sel-610.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codice nazione               *
      *              *-------------------------------------------------*
           perform   reg-ric-sel-naz-000  thru reg-ric-sel-naz-999    .
      *                  *---------------------------------------------*
      *                  * Ad ulteriori regolarizzazioni               *
      *                  *---------------------------------------------*
           go to     reg-ric-sel-800.
       reg-ric-sel-620.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codice regione               *
      *              *-------------------------------------------------*
           perform   reg-ric-sel-rgn-000  thru reg-ric-sel-rgn-999    .
      *                  *---------------------------------------------*
      *                  * Ad ulteriori regolarizzazioni               *
      *                  *---------------------------------------------*
           go to     reg-ric-sel-800.
       reg-ric-sel-630.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codice provincia             *
      *              *-------------------------------------------------*
           perform   reg-ric-sel-prv-000  thru reg-ric-sel-prv-999    .
      *                  *---------------------------------------------*
      *                  * Ad ulteriori regolarizzazioni               *
      *                  *---------------------------------------------*
           go to     reg-ric-sel-800.
       reg-ric-sel-640.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codice comune                *
      *              *-------------------------------------------------*
           perform   reg-ric-sel-cmn-000  thru reg-ric-sel-cmn-999    .
      *                  *---------------------------------------------*
      *                  * Ad ulteriori regolarizzazioni               *
      *                  *---------------------------------------------*
           go to     reg-ric-sel-800.
       reg-ric-sel-710.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codici avviamento postale    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad ulteriori regolarizzazioni               *
      *                  *---------------------------------------------*
           go to     reg-ric-sel-800.
       reg-ric-sel-800.
      *              *-------------------------------------------------*
      *              * Tipo ordinamento fornitori a parita' di anagra- *
      *              * fica di ordinamento                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se necessario                          *
      *                  *---------------------------------------------*
           if        rr-tip-ors           =    01
                     go to reg-ric-sel-820.
      *                  *---------------------------------------------*
      *                  * Regolarizzazione                            *
      *                  *---------------------------------------------*
           if        rr-tip-ocp           =    zero
                     move  01             to   rr-tip-ocp             .
       reg-ric-sel-820.
      *              *-------------------------------------------------*
      *              * Tipo stampa                                     *
      *              *-------------------------------------------------*
           if        rr-tip-stp           =    zero
                     move  01             to   rr-tip-stp             .
       reg-ric-sel-830.
      *              *-------------------------------------------------*
      *              * Tipo di etichette                               *
      *              *-------------------------------------------------*
           if        rr-tip-stp           not  = 31 and
                     rr-tip-stp           not  = 32
                     go to reg-ric-sel-840.
           if        rr-tip-eti           =    zero
                     move  01             to   rr-tip-eti             .
       reg-ric-sel-840.
      *              *-------------------------------------------------*
      *              * Voci aggiuntive da stampare                     *
      *              *-------------------------------------------------*
           if        rr-tip-stp           not  = 11 and
                     rr-tip-stp           not  = 51
                     move  spaces         to   rr-vag-dst             .
       reg-ric-sel-860.
      *              *-------------------------------------------------*
      *              * Si/no salto pagina a rottura                    *
      *              *-------------------------------------------------*
           if        rr-sns-prt           =    zero
                     move  01             to   rr-sns-prt             .
       reg-ric-sel-890.
      *              *-------------------------------------------------*
      *              * Status commerciale                              *
      *              *-------------------------------------------------*
           if        rr-ssu-sta-tus       =    spaces
                     move  "X        "    to   rr-ssu-sta-tus         .
       reg-ric-sel-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     reg-ric-sel-999.
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione dei parametri di selezione               *
      *    *                                                           *
      *    * Fornitore                                                 *
      *    *-----------------------------------------------------------*
       reg-ric-sel-fnt-000.
      *              *-------------------------------------------------*
      *              * Codice fornitore iniziale e finale              *
      *              *-------------------------------------------------*
           if        rr-sel-fnt-coi       =    zero and
                     rr-sel-fnt-cof       =    zero
                     move   9999999       to   rr-sel-fnt-cof
           else if   rr-sel-fnt-coi       not  = zero and
                     rr-sel-fnt-cof       =    zero
                     move  rr-sel-fnt-coi to   rr-sel-fnt-cof         .
       reg-ric-sel-fnt-200.
      *              *-------------------------------------------------*
      *              * Ragione sociale fornitore iniziale e finale     *
      *              *-------------------------------------------------*
           if        rr-sel-fnt-rsf       =    spaces
                     move  rr-sel-fnt-rsi to   rr-sel-fnt-rsf         .
           move      rr-sel-fnt-rsf       to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   rr-sel-fnt-rsf         .
       reg-ric-sel-fnt-400.
      *              *-------------------------------------------------*
      *              * Mnemonico fornitore iniziale e finale           *
      *              *-------------------------------------------------*
           if        rr-sel-fnt-mnf       =    spaces
                     move  rr-sel-fnt-mni to   rr-sel-fnt-mnf         .
           move      rr-sel-fnt-mnf       to   w-all-str-alf          .
           move      10                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   rr-sel-fnt-mnf         .
       reg-ric-sel-fnt-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione dei parametri di selezione               *
      *    *                                                           *
      *    * Zona                                                      *
      *    *-----------------------------------------------------------*
       reg-ric-sel-zon-000.
      *              *-------------------------------------------------*
      *              * Regolarizzazione descrizione min - max          *
      *              *-------------------------------------------------*
           if        rr-sel-zon-def       =    spaces
                     move  rr-sel-zon-dei to   rr-sel-zon-def         .
           move      rr-sel-zon-def       to   w-all-str-alf          .
           move      20                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   rr-sel-zon-def         .
       reg-ric-sel-zon-200.
      *              *-------------------------------------------------*
      *              * Regolarizzazione codice min - max               *
      *              *-------------------------------------------------*
           if        rr-sel-zon-coi       =    zero and
                     rr-sel-zon-cof       =    zero
                     move   99999         to   rr-sel-zon-cof
           else if   rr-sel-zon-coi       not  = zero and
                     rr-sel-zon-cof       =    zero
                     move  rr-sel-zon-coi to   rr-sel-zon-cof         .
       reg-ric-sel-zon-400.
      *              *-------------------------------------------------*
      *              * Regolarizzazione mnemonico min - max            *
      *              *-------------------------------------------------*
           if        rr-sel-zon-mnf       =    spaces
                     move  rr-sel-zon-mni to   rr-sel-zon-mnf         .
           move      rr-sel-zon-mnf       to   w-all-str-alf          .
           move      05                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   rr-sel-zon-mnf         .
       reg-ric-sel-zon-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione dei parametri di selezione               *
      *    *                                                           *
      *    * Categoria                                                 *
      *    *-----------------------------------------------------------*
       reg-ric-sel-cat-000.
      *              *-------------------------------------------------*
      *              * Regolarizzazione descrizione min - max          *
      *              *-------------------------------------------------*
           if        rr-sel-cat-def       =    spaces
                     move  rr-sel-cat-dei to   rr-sel-cat-def         .
           move      rr-sel-cat-def       to   w-all-str-alf          .
           move      20                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   rr-sel-cat-def         .
       reg-ric-sel-cat-200.
      *              *-------------------------------------------------*
      *              * Regolarizzazione codice min - max               *
      *              *-------------------------------------------------*
           if        rr-sel-cat-coi       =    zero and
                     rr-sel-cat-cof       =    zero
                     move   99999         to   rr-sel-cat-cof
           else if   rr-sel-cat-coi       not  = zero and
                     rr-sel-cat-cof       =    zero
                     move  rr-sel-cat-coi to   rr-sel-cat-cof         .
       reg-ric-sel-cat-400.
      *              *-------------------------------------------------*
      *              * Regolarizzazione mnemonico min - max            *
      *              *-------------------------------------------------*
           if        rr-sel-cat-mnf       =    spaces
                     move  rr-sel-cat-mni to   rr-sel-cat-mnf         .
           move      rr-sel-cat-mnf       to   w-all-str-alf          .
           move      05                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   rr-sel-cat-mnf         .
       reg-ric-sel-cat-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione dei parametri di selezione               *
      *    *                                                           *
      *    * Codice statistico                                         *
      *    *-----------------------------------------------------------*
       reg-ric-sel-stt-000.
      *              *-------------------------------------------------*
      *              * Regolarizzazione descrizione min - max          *
      *              *-------------------------------------------------*
           if        rr-sel-stt-def       =    spaces
                     move  rr-sel-stt-dei to   rr-sel-stt-def         .
           move      rr-sel-stt-def       to   w-all-str-alf          .
           move      20                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   rr-sel-stt-def         .
       reg-ric-sel-stt-200.
      *              *-------------------------------------------------*
      *              * Regolarizzazione codice min - max               *
      *              *-------------------------------------------------*
           if        rr-sel-stt-coi       =    zero and
                     rr-sel-stt-cof       =    zero
                     move   99999         to   rr-sel-stt-cof
           else if   rr-sel-stt-coi       not  = zero and
                     rr-sel-stt-cof       =    zero
                     move  rr-sel-stt-coi to   rr-sel-stt-cof         .
       reg-ric-sel-stt-400.
      *              *-------------------------------------------------*
      *              * Regolarizzazione mnemonico min - max            *
      *              *-------------------------------------------------*
           if        rr-sel-stt-mnf       =    spaces
                     move  rr-sel-stt-mni to   rr-sel-stt-mnf         .
           move      rr-sel-stt-mnf       to   w-all-str-alf          .
           move      05                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   rr-sel-stt-mnf         .
       reg-ric-sel-stt-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione dei parametri di selezione               *
      *    *                                                           *
      *    * Forma di pagamento                                        *
      *    *-----------------------------------------------------------*
       reg-ric-sel-fop-000.
      *              *-------------------------------------------------*
      *              * Regolarizzazione descrizione min - max          *
      *              *-------------------------------------------------*
           if        rr-sel-fop-def       =    spaces
                     move  rr-sel-fop-dei to   rr-sel-fop-def         .
           move      rr-sel-fop-def       to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   rr-sel-fop-def         .
       reg-ric-sel-fop-200.
      *              *-------------------------------------------------*
      *              * Regolarizzazione codice min - max               *
      *              *-------------------------------------------------*
           if        rr-sel-fop-coi       =    zero and
                     rr-sel-fop-cof       =    zero
                     move   99999         to   rr-sel-fop-cof
           else if   rr-sel-fop-coi       not  = zero and
                     rr-sel-fop-cof       =    zero
                     move  rr-sel-fop-coi to   rr-sel-fop-cof         .
       reg-ric-sel-fop-400.
      *              *-------------------------------------------------*
      *              * Regolarizzazione mnemonico min - max            *
      *              *-------------------------------------------------*
           if        rr-sel-fop-mnf       =    spaces
                     move  rr-sel-fop-mni to   rr-sel-fop-mnf         .
           move      rr-sel-fop-mnf       to   w-all-str-alf          .
           move      10                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   rr-sel-fop-mnf         .
       reg-ric-sel-fop-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione dei parametri di selezione               *
      *    *                                                           *
      *    * Codice nazione                                            *
      *    *-----------------------------------------------------------*
       reg-ric-sel-naz-000.
      *              *-------------------------------------------------*
      *              * Regolarizzazione descrizione min - max          *
      *              *-------------------------------------------------*
           if        rr-sel-naz-def       =    spaces
                     move  rr-sel-naz-dei to   rr-sel-naz-def         .
           move      rr-sel-naz-def       to   w-all-str-alf          .
           move      25                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   rr-sel-naz-def         .
       reg-ric-sel-naz-200.
      *              *-------------------------------------------------*
      *              * Regolarizzazione codice min - max               *
      *              *-------------------------------------------------*
           if        rr-sel-naz-coi       =    spaces and
                     rr-sel-naz-cof       =    spaces
                     move   "zzz"         to   rr-sel-naz-cof
           else if   rr-sel-naz-coi       not  = spaces and
                     rr-sel-naz-cof       =    spaces
                     move  rr-sel-naz-coi to   rr-sel-naz-cof         .
       reg-ric-sel-naz-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione dei parametri di selezione               *
      *    *                                                           *
      *    * Codice regione                                            *
      *    *-----------------------------------------------------------*
       reg-ric-sel-rgn-000.
      *              *-------------------------------------------------*
      *              * Regolarizzazione descrizione min - max          *
      *              *-------------------------------------------------*
           if        rr-sel-rgn-def       =    spaces
                     move  rr-sel-rgn-dei to   rr-sel-rgn-def         .
           move      rr-sel-rgn-def       to   w-all-str-alf          .
           move      25                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   rr-sel-rgn-def         .
       reg-ric-sel-rgn-200.
      *              *-------------------------------------------------*
      *              * Regolarizzazione codice min - max               *
      *              *-------------------------------------------------*
           if        rr-sel-rgn-coi       =    spaces and
                     rr-sel-rgn-cof       =    spaces
                     move   "zzz"         to   rr-sel-rgn-cof
           else if   rr-sel-rgn-coi       not  = spaces and
                     rr-sel-rgn-cof       =    spaces
                     move  rr-sel-rgn-coi to   rr-sel-rgn-cof         .
       reg-ric-sel-rgn-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione dei parametri di selezione               *
      *    *                                                           *
      *    * Codice provincia                                          *
      *    *-----------------------------------------------------------*
       reg-ric-sel-prv-000.
      *              *-------------------------------------------------*
      *              * Regolarizzazione descrizione min - max          *
      *              *-------------------------------------------------*
           if        rr-sel-prv-def       =    spaces
                     move  rr-sel-prv-dei to   rr-sel-prv-def         .
           move      rr-sel-prv-def       to   w-all-str-alf          .
           move      25                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   rr-sel-prv-def         .
       reg-ric-sel-prv-200.
      *              *-------------------------------------------------*
      *              * Regolarizzazione codice min - max               *
      *              *-------------------------------------------------*
           if        rr-sel-prv-coi       =    spaces and
                     rr-sel-prv-cof       =    spaces
                     move   "zzz"         to   rr-sel-prv-cof
           else if   rr-sel-prv-coi       not  = spaces and
                     rr-sel-prv-cof       =    spaces
                     move  rr-sel-prv-coi to   rr-sel-prv-cof         .
       reg-ric-sel-prv-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione dei parametri di selezione               *
      *    *                                                           *
      *    * Codice comune                                             *
      *    *-----------------------------------------------------------*
       reg-ric-sel-cmn-000.
      *              *-------------------------------------------------*
      *              * Regolarizzazione descrizione min - max          *
      *              *-------------------------------------------------*
           if        rr-sel-cmn-def       =    spaces
                     move  rr-sel-cmn-dei to   rr-sel-cmn-def         .
           move      rr-sel-cmn-def       to   w-all-str-alf          .
           move      30                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   rr-sel-cmn-def         .
       reg-ric-sel-cmn-200.
      *              *-------------------------------------------------*
      *              * Regolarizzazione C.a.p. min - max               *
      *              *-------------------------------------------------*
           if        rr-sel-cmn-cpi       =    spaces and
                     rr-sel-cmn-cpf       =    spaces
                     move   "zzzzz"       to   rr-sel-cmn-cpf
           else if   rr-sel-cmn-cpi       not  = spaces and
                     rr-sel-cmn-cpf       =    spaces
                     move  rr-sel-cmn-cpi to   rr-sel-cmn-cpf         .
       reg-ric-sel-cmn-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Tipo ordinamento stampa                         *
      *              *-------------------------------------------------*
           move      zero                 to   rr-tip-ors             .
      *              *-------------------------------------------------*
      *              * Tipi ordinamento su anagrafiche                 *
      *              *-------------------------------------------------*
           perform   nor-ric-sel-ord-000  thru nor-ric-sel-ord-999    .
      *              *-------------------------------------------------*
      *              * Selettori su anagrafica fornitori               *
      *              *-------------------------------------------------*
           perform   nor-ric-sel-fnt-000  thru nor-ric-sel-fnt-999    .
      *              *-------------------------------------------------*
      *              * Selettori su anagrafica zone                    *
      *              *-------------------------------------------------*
           perform   nor-ric-sel-zon-000  thru nor-ric-sel-zon-999    .
      *              *-------------------------------------------------*
      *              * Selettori su anagrafica categorie               *
      *              *-------------------------------------------------*
           perform   nor-ric-sel-cat-000  thru nor-ric-sel-cat-999    .
      *              *-------------------------------------------------*
      *              * Selettori su anagrafica codici statistici       *
      *              *-------------------------------------------------*
           perform   nor-ric-sel-stt-000  thru nor-ric-sel-stt-999    .
      *              *-------------------------------------------------*
      *              * Selettori su anagrafica forme di pagamento      *
      *              *-------------------------------------------------*
           perform   nor-ric-sel-fop-000  thru nor-ric-sel-fop-999    .
      *              *-------------------------------------------------*
      *              * Selettori su anagrafica nazioni                 *
      *              *-------------------------------------------------*
           perform   nor-ric-sel-naz-000  thru nor-ric-sel-naz-999    .
      *              *-------------------------------------------------*
      *              * Selettori su anagrafica regioni                 *
      *              *-------------------------------------------------*
           perform   nor-ric-sel-rgn-000  thru nor-ric-sel-rgn-999    .
      *              *-------------------------------------------------*
      *              * Selettori su anagrafica provincie               *
      *              *-------------------------------------------------*
           perform   nor-ric-sel-prv-000  thru nor-ric-sel-prv-999    .
      *              *-------------------------------------------------*
      *              * Selettori su anagrafica comuni                  *
      *              *-------------------------------------------------*
           perform   nor-ric-sel-cmn-000  thru nor-ric-sel-cmn-999    .
      *              *-------------------------------------------------*
      *              * Selettori supplementari                         *
      *              *-------------------------------------------------*
           perform   nor-ric-sel-ssu-000  thru nor-ric-sel-ssu-999    .
      *              *-------------------------------------------------*
      *              * Tipo ordinamento fornitori a parita' di anagra- *
      *              * fica di ordinamento                             *
      *              *-------------------------------------------------*
           perform   nor-ric-sel-ocp-000  thru nor-ric-sel-ocp-999    .
      *              *-------------------------------------------------*
      *              * Tipo stampa richiesta                           *
      *              *-------------------------------------------------*
           move      zero                 to   rr-tip-stp             .
      *              *-------------------------------------------------*
      *              * Tipo di etichette                               *
      *              *-------------------------------------------------*
           move      zero                 to   rr-tip-eti             .
      *              *-------------------------------------------------*
      *              * Voci aggiuntive da stampare                     *
      *              *-------------------------------------------------*
           move      spaces               to   rr-vag-dst             .
      *              *-------------------------------------------------*
      *              * Si/no salto pagina a rottura                    *
      *              *-------------------------------------------------*
           move      zero                 to   rr-sns-prt             .
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *                                                           *
      *    * Tipi ordinamento su anagrafiche                           *
      *    *-----------------------------------------------------------*
       nor-ric-sel-ord-000.
           move      zero                 to   rr-ord-ana-fnt         .
           move      zero                 to   rr-ord-ana-zon         .
           move      zero                 to   rr-ord-ana-cat         .
           move      zero                 to   rr-ord-ana-stt         .
           move      zero                 to   rr-ord-ana-fop         .
           move      zero                 to   rr-ord-ana-naz         .
           move      zero                 to   rr-ord-ana-rgn         .
           move      zero                 to   rr-ord-ana-prv         .
           move      zero                 to   rr-ord-ana-cmn         .
       nor-ric-sel-ord-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *                                                           *
      *    * Selettori su anagrafica fornitori                         *
      *    *-----------------------------------------------------------*
       nor-ric-sel-fnt-000.
           move      spaces               to   rr-sel-fnt-rsi         .
           move      spaces               to   rr-sel-fnt-rsf         .
           move      zero                 to   rr-sel-fnt-coi         .
           move      zero                 to   rr-sel-fnt-cof         .
           move      spaces               to   rr-sel-fnt-mni         .
           move      spaces               to   rr-sel-fnt-mnf         .
           move      spaces               to   rr-sel-fnt-dpz         .
       nor-ric-sel-fnt-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *                                                           *
      *    * Selettori su anagrafica zone                              *
      *    *-----------------------------------------------------------*
       nor-ric-sel-zon-000.
           move      spaces               to   rr-sel-zon-dei         .
           move      spaces               to   rr-sel-zon-def         .
           move      zero                 to   rr-sel-zon-coi         .
           move      zero                 to   rr-sel-zon-cof         .
           move      spaces               to   rr-sel-zon-mni         .
           move      spaces               to   rr-sel-zon-mnf         .
       nor-ric-sel-zon-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *                                                           *
      *    * Selettori su anagrafica categorie                         *
      *    *-----------------------------------------------------------*
       nor-ric-sel-cat-000.
           move      spaces               to   rr-sel-cat-dei         .
           move      spaces               to   rr-sel-cat-def         .
           move      zero                 to   rr-sel-cat-coi         .
           move      zero                 to   rr-sel-cat-cof         .
           move      spaces               to   rr-sel-cat-mni         .
           move      spaces               to   rr-sel-cat-mnf         .
       nor-ric-sel-cat-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *                                                           *
      *    * Selettori su anagrafica codici statistici                 *
      *    *-----------------------------------------------------------*
       nor-ric-sel-stt-000.
           move      spaces               to   rr-sel-stt-dei         .
           move      spaces               to   rr-sel-stt-def         .
           move      zero                 to   rr-sel-stt-coi         .
           move      zero                 to   rr-sel-stt-cof         .
           move      spaces               to   rr-sel-stt-mni         .
           move      spaces               to   rr-sel-stt-mnf         .
       nor-ric-sel-stt-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *                                                           *
      *    * Selettori su anagrafica forme di pagamento                *
      *    *-----------------------------------------------------------*
       nor-ric-sel-fop-000.
           move      spaces               to   rr-sel-fop-dei         .
           move      spaces               to   rr-sel-fop-def         .
           move      zero                 to   rr-sel-fop-coi         .
           move      zero                 to   rr-sel-fop-cof         .
           move      spaces               to   rr-sel-fop-mni         .
           move      spaces               to   rr-sel-fop-mnf         .
       nor-ric-sel-fop-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *                                                           *
      *    * Selettori su anagrafica codici nazione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-naz-000.
           move      spaces               to   rr-sel-naz-dei         .
           move      spaces               to   rr-sel-naz-def         .
           move      spaces               to   rr-sel-naz-coi         .
           move      spaces               to   rr-sel-naz-cof         .
       nor-ric-sel-naz-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *                                                           *
      *    * Selettori su anagrafica codici regione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-rgn-000.
           move      spaces               to   rr-sel-rgn-dei         .
           move      spaces               to   rr-sel-rgn-def         .
           move      spaces               to   rr-sel-rgn-coi         .
           move      spaces               to   rr-sel-rgn-cof         .
       nor-ric-sel-rgn-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *                                                           *
      *    * Selettori su anagrafica codici provincia                  *
      *    *-----------------------------------------------------------*
       nor-ric-sel-prv-000.
           move      spaces               to   rr-sel-prv-dei         .
           move      spaces               to   rr-sel-prv-def         .
           move      spaces               to   rr-sel-prv-coi         .
           move      spaces               to   rr-sel-prv-cof         .
       nor-ric-sel-prv-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *                                                           *
      *    * Selettori su anagrafica codici comune                     *
      *    *-----------------------------------------------------------*
       nor-ric-sel-cmn-000.
           move      spaces               to   rr-sel-cmn-dei         .
           move      spaces               to   rr-sel-cmn-def         .
           move      spaces               to   rr-sel-cmn-cpi         .
           move      spaces               to   rr-sel-cmn-cpf         .
       nor-ric-sel-cmn-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *                                                           *
      *    * Selettori supplementari                                   *
      *    *-----------------------------------------------------------*
       nor-ric-sel-ssu-000.
           move      spaces               to   rr-ssu-cod-cbp         .
           move      spaces               to   rr-ssu-cod-cbp-des     .
           move      zero                 to   rr-ssu-cod-zon         .
           move      spaces               to   rr-ssu-cod-zon-des     .
           move      zero                 to   rr-ssu-cod-cat         .
           move      spaces               to   rr-ssu-cod-cat-des     .
           move      zero                 to   rr-ssu-cod-stt         .
           move      spaces               to   rr-ssu-cod-stt-des     .
           move      spaces               to   rr-ssu-sta-tus         .
           move      spaces               to   rr-ssu-cod-cap (1)     .
           move      spaces               to   rr-ssu-cod-cap (2)     .
           move      spaces               to   rr-ssu-cod-cap (3)     .
           move      spaces               to   rr-ssu-cod-cap (4)     .
           move      spaces               to   rr-ssu-cod-cap (5)     .
           move      spaces               to   rr-ssu-cod-cap (6)     .
       nor-ric-sel-ssu-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *                                                           *
      *    * Tipo ordinamento fornitori a parita' di anagrafica di     *
      *    * ordinamento                                               *
      *    *-----------------------------------------------------------*
       nor-ric-sel-ocp-000.
           move      zero                 to   rr-tip-ocp             .
       nor-ric-sel-ocp-999.
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
           go to     let-arc-cbp-999.
       let-arc-cbp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cbp-des      .
       let-arc-cbp-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [yst]                         *
      *    *-----------------------------------------------------------*
       let-arc-yst-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-yst-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice                                  *
      *              *-------------------------------------------------*
           if        w-let-arc-yst-cod    =    zero
                     go to let-arc-yst-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLS    "         to   f-key                  .
           move      w-let-arc-yst-tip    to   rf-yst-tip-cls         .
           move      w-let-arc-yst-cod    to   rf-yst-cod-cls         .
           move      "pgm/dcf/fls/ioc/obj/iofyst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yst                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-yst-400.
       let-arc-yst-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-yst-des-cls       to   w-let-arc-yst-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-yst-999.
       let-arc-yst-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-yst-flg      .
           move      all   "."            to   w-let-arc-yst-des      .
           go to     let-arc-yst-999.
       let-arc-yst-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-yst-des      .
       let-arc-yst-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice fornitore com-  *
      *    * merciale                                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmndcf0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice nostra cassa, o *
      *    * nostra banca, o nostro c/c postale                        *
      *    *-----------------------------------------------------------*
           copy      "pgm/gep/prg/cpy/acdecbp0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice zona            *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmnyst1.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice categoria       *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmnyst2.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice statistico      *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmnyst3.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione della forma di pagamento   *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmnyfp0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice nazione         *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/acdenaz0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice regione         *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/acdergn0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice provincia       *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/acdeprv0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice Comune 'geo'        *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/acomgeo0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice Frazione 'geo'      *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/afrageo0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice Localita' 'geo'     *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/alocgeo0.acs"                   .

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
      *    * Preparazione parametri per selezione stampa               *
      *    *-----------------------------------------------------------*
       pre-prm-stp-000.
      *              *-------------------------------------------------*
      *              * Assestamento del pathname del programma da ese- *
      *              * guire in base al tipo stampa                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione campi di comodo             *
      *                  *---------------------------------------------*
           move      spaces               to   w-ovy-exe-pro          .
           move      spaces               to   w-ovy-exe-pat          .
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo di stampa   *
      *                  *---------------------------------------------*
           if        rr-tip-stp           =    01 or
                     rr-tip-stp           =    11
                     move  "pdcf4021  "   to   w-ovy-exe-pro
                     move  "pgm/dcf/prg/obj/pdcf4021  "
                                          to   w-ovy-exe-pat
           else if   rr-tip-stp           =    21
                     move  "pdcf4022  "   to   w-ovy-exe-pro
                     move  "pgm/dcf/prg/obj/pdcf4022  "
                                          to   w-ovy-exe-pat
           else      move  "pdcf4023  "   to   w-ovy-exe-pro
                     move  "pgm/dcf/prg/obj/pdcf4023  "
                                          to   w-ovy-exe-pat          .
      *                  *---------------------------------------------*
      *                  * Sigla interna del programma di esecuzione   *
      *                  *---------------------------------------------*
           move      w-ovy-exe-pro        to   i-exe-pro              .
      *                  *---------------------------------------------*
      *                  * Pathname del programma di esecuzione        *
      *                  *---------------------------------------------*
           move      w-ovy-exe-pat        to   i-exe-pat              .
       pre-prm-stp-100.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di stampa       *
      *              *-------------------------------------------------*
           if        rr-tip-stp           =    01
                     perform  pre-prm-stp-001-000
                                          thru pre-prm-stp-001-999
           else if   rr-tip-stp           =    11
                     perform  pre-prm-stp-001-000
                                          thru pre-prm-stp-001-999
           else if   rr-tip-stp           =    21
                     perform  pre-prm-stp-021-000
                                          thru pre-prm-stp-021-999
           else if   rr-tip-stp           =    31
                     perform  pre-prm-stp-031-000
                                          thru pre-prm-stp-031-999
           else if   rr-tip-stp           =    32
                     perform  pre-prm-stp-031-000
                                          thru pre-prm-stp-031-999
           else      perform  pre-prm-stp-001-000
                                          thru pre-prm-stp-001-999    .
       pre-prm-stp-999.
           exit.

      *    *===========================================================*
      *    * Preparazione parametri per stampa elenco anagrafiche      *
      *    *-----------------------------------------------------------*
       pre-prm-stp-001-000.
      *              *-------------------------------------------------*
      *              * Eventuale aggiustamento flag di richiesta       *
      *              * selezione stampa in caso di archivio sequenzia- *
      *              * le                                              *
      *              *-------------------------------------------------*
           if        rr-tip-stp           =    51
                     move  "N"            to   w-cnt-fun-snx-stp
                     go to pre-prm-stp-001-999.
       pre-prm-stp-001-100.
      *              *-------------------------------------------------*
      *              * Flags di tipo selezione                         *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-tip-sel      .
      *              *-------------------------------------------------*
      *              * Codice stampante                                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-cod-stp      .
      *              *-------------------------------------------------*
      *              * Tipo di stampa                                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-tip-sta      .
      *              *-------------------------------------------------*
      *              * Codice modulo                                   *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-cod-mod      .
      *              *-------------------------------------------------*
      *              * Tipo modulo                                     *
      *              *   - L : Libero                                  *
      *              *   - T : Tipografico                             *
      *              *-------------------------------------------------*
           move      "L"                  to   w-cnt-stp-tip-mod      .
      *              *-------------------------------------------------*
      *              * Ampiezza linea di stampa in caratteri           *
      *              *-------------------------------------------------*
           move      132                  to   w-cnt-stp-amp-lin      .
      *              *-------------------------------------------------*
      *              * Top margin in linee                             *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-stp-top-lin      .
      *              *-------------------------------------------------*
      *              * Numero linee di stampa minimo                   *
      *              *-------------------------------------------------*
           move      30                   to   w-cnt-stp-lin-min      .
      *              *-------------------------------------------------*
      *              * Bottom margin in linee                          *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-stp-bot-lin      .
      *              *-------------------------------------------------*
      *              * Ampiezza caratteri                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stp-amp-car      .
      *              *-------------------------------------------------*
      *              * Altezza interlinea                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stp-alt-int      .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni future            *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-esp-fut      .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni speciali          *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-fnz-spc      .
       pre-prm-stp-001-999.
           exit.

      *    *===========================================================*
      *    * Preparazione parametri per stampa scheda completa         *
      *    *-----------------------------------------------------------*
       pre-prm-stp-021-000.
      *              *-------------------------------------------------*
      *              * Flags di tipo selezione                         *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-tip-sel      .
      *              *-------------------------------------------------*
      *              * Codice stampante                                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-cod-stp      .
      *              *-------------------------------------------------*
      *              * Tipo di stampa                                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-tip-sta      .
      *              *-------------------------------------------------*
      *              * Codice modulo                                   *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-cod-mod      .
      *              *-------------------------------------------------*
      *              * Tipo modulo                                     *
      *              *   - L : Libero                                  *
      *              *   - T : Tipografico                             *
      *              *-------------------------------------------------*
           move      "L"                  to   w-cnt-stp-tip-mod      .
      *              *-------------------------------------------------*
      *              * Ampiezza linea di stampa in caratteri           *
      *              *-------------------------------------------------*
           move      096                  to   w-cnt-stp-amp-lin      .
      *              *-------------------------------------------------*
      *              * Top margin in linee                             *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-stp-top-lin      .
      *              *-------------------------------------------------*
      *              * Numero linee di stampa minimo                   *
      *              *-------------------------------------------------*
           move      62                   to   w-cnt-stp-lin-min      .
      *              *-------------------------------------------------*
      *              * Bottom margin in linee                          *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-stp-bot-lin      .
      *              *-------------------------------------------------*
      *              * Ampiezza caratteri                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stp-amp-car      .
      *              *-------------------------------------------------*
      *              * Altezza interlinea                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stp-alt-int      .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni future            *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-esp-fut      .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni speciali          *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-fnz-spc      .
       pre-prm-stp-021-999.
           exit.

      *    *===========================================================*
      *    * Preparazione parametri per stampa etichette               *
      *    *-----------------------------------------------------------*
       pre-prm-stp-031-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di etichette    *
      *              *-------------------------------------------------*
           if        rr-tip-eti           =    01
                     perform  pre-prm-stp-301-000
                                          thru pre-prm-stp-301-999
           else if   rr-tip-eti           =    51
                     perform  pre-prm-stp-351-000
                                          thru pre-prm-stp-351-999
           else      perform  pre-prm-stp-301-000
                                          thru pre-prm-stp-301-999    .
       pre-prm-stp-031-999.
           exit.

      *    *===========================================================*
      *    * Preparazione parametri per stampa etichette tipo 01       *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    *  - Modulo stampa  : continuo                              *
      *    *  - Lunghezza mod. : 12''          (864 72.mi)             *
      *    *  - Larghezza mod. : 4''           (288 72.mi)             *
      *    *  - Numero colonne : 01                                    *
      *    *  - Lunghezza  mm. : 100                                   *
      *    *  - Altezza    mm. : 036                                   *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       pre-prm-stp-301-000.
      *              *-------------------------------------------------*
      *              * Flags di tipo selezione                         *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-tip-sel      .
      *              *-------------------------------------------------*
      *              * Codice stampante                                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-cod-stp      .
      *              *-------------------------------------------------*
      *              * Tipo di stampa                                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-tip-sta      .
      *              *-------------------------------------------------*
      *              * Codice modulo                                   *
      *              *-------------------------------------------------*
           move      "eticli  "           to   w-cnt-stp-cod-mod      .
      *              *-------------------------------------------------*
      *              * Tipo modulo                                     *
      *              *   - L : Libero                                  *
      *              *   - T : Tipografico                             *
      *              *-------------------------------------------------*
           move      "T"                  to   w-cnt-stp-tip-mod      .
      *              *-------------------------------------------------*
      *              * Ampiezza linea di stampa in caratteri           *
      *              *-------------------------------------------------*
           move      48                   to   w-cnt-stp-amp-lin      .
      *              *-------------------------------------------------*
      *              * Top margin in linee                             *
      *              *-------------------------------------------------*
           move      0                    to   w-cnt-stp-top-lin      .
      *              *-------------------------------------------------*
      *              * Numero linee di stampa minimo                   *
      *              *-------------------------------------------------*
           move      72                   to   w-cnt-stp-lin-min      .
      *              *-------------------------------------------------*
      *              * Bottom margin in linee                          *
      *              *-------------------------------------------------*
           move      0                    to   w-cnt-stp-bot-lin      .
      *              *-------------------------------------------------*
      *              * Ampiezza caratteri                              *
      *              *-------------------------------------------------*
           move      12,00                to   w-cnt-stp-amp-car      .
      *              *-------------------------------------------------*
      *              * Altezza interlinea                              *
      *              *-------------------------------------------------*
           move      06,00                to   w-cnt-stp-alt-int      .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni future            *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-esp-fut      .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni speciali          *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-fnz-spc      .
       pre-prm-stp-301-999.
           exit.

      *    *===========================================================*
      *    * Preparazione parametri per stampa etichette tipo 02       *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    *  - Modulo stampa  : Laser A4                              *
      *    *  - Lunghezza mod. :                                       *
      *    *  - Larghezza mod. :                                       *
      *    *  - Numero colonne : 03                                    *
      *    *  - Lunghezza  mm. : 070                                   *
      *    *  - Altezza    mm. : 036                                   *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       pre-prm-stp-351-000.
      *              *-------------------------------------------------*
      *              * Flags di tipo selezione                         *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-tip-sel      .
      *              *-------------------------------------------------*
      *              * Codice stampante                                *
      *              *-------------------------------------------------*
           move      "etilaser"           to   w-cnt-stp-cod-stp      .
      *              *-------------------------------------------------*
      *              * Tipo di stampa                                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-tip-sta      .
      *              *-------------------------------------------------*
      *              * Codice modulo                                   *
      *              *-------------------------------------------------*
           move      "etilaser"           to   w-cnt-stp-cod-mod      .
      *              *-------------------------------------------------*
      *              * Tipo modulo                                     *
      *              *   - L : Libero                                  *
      *              *   - T : Tipografico                             *
      *              *-------------------------------------------------*
           move      "T"                  to   w-cnt-stp-tip-mod      .
      *              *-------------------------------------------------*
      *              * Ampiezza linea di stampa in caratteri           *
      *              *-------------------------------------------------*
           move      132                  to   w-cnt-stp-amp-lin      .
      *              *-------------------------------------------------*
      *              * Top margin in linee                             *
      *              *-------------------------------------------------*
           move      0                    to   w-cnt-stp-top-lin      .
      *              *-------------------------------------------------*
      *              * Numero linee di stampa minimo                   *
      *              *-------------------------------------------------*
           move      72                   to   w-cnt-stp-lin-min      .
      *              *-------------------------------------------------*
      *              * Bottom margin in linee                          *
      *              *-------------------------------------------------*
           move      0                    to   w-cnt-stp-bot-lin      .
      *              *-------------------------------------------------*
      *              * Ampiezza caratteri                              *
      *              *-------------------------------------------------*
           move      20,00                to   w-cnt-stp-amp-car      .
      *              *-------------------------------------------------*
      *              * Altezza interlinea                              *
      *              *-------------------------------------------------*
           move      06,00                to   w-cnt-stp-alt-int      .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni future            *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-esp-fut      .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni speciali          *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-fnz-spc      .
       pre-prm-stp-351-999.
           exit.

      *    *===========================================================*
      *    * Lettura i.p.c. da livello precedente per stampa scheda    *
      *    * fornitore da programma di gestione                        *
      *    *-----------------------------------------------------------*
       ipc-dlp-tdi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione iniziale                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-ipc-dlp-stp-fnt      .
           move      spaces               to   w-ipc-dlp-stp-dpz      .
      *              *-------------------------------------------------*
      *              * Estrazione della eventuale variabile di i.p.c.  *
      *              * "cod-fnt" per il codice fornitore               *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-fnt"            to   s-var                  .
           move      "-"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     go to ipc-dlp-tdi-999.
           if        s-num                =    zero
                     go to ipc-dlp-tdi-999.
           move      s-num                to   w-ipc-dlp-stp-fnt      .
       ipc-dlp-tdi-600.
      *              *-------------------------------------------------*
      *              * Estrazione della eventuale variabile di i.p.c.  *
      *              * "dpz-fnt" per la dipendenza                     *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "dpz-fnt"            to   s-var                  .
           move      "-"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     go to ipc-dlp-tdi-999.
           if        s-alf                =    spaces
                     go to ipc-dlp-tdi-999.
           move      s-alf                to   w-ipc-dlp-stp-dpz      .
           go to     ipc-dlp-tdi-999.
       ipc-dlp-tdi-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

