       Identification Division.
       Program-Id.                                 zttadc03           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:                        *
      *                                   Fase:    zttadc              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 20/02/11    *
      *                       Ultima revisione:    NdK del 20/02/11    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Aggiornamento file [adc]                    *
      *                                                                *
      *                    Cancellazione utenze mail clienti commerc.  *
      *                                                                *
      *                    File preparato con 't_format_cbl'           *
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
      *            * Tipo archivio                                     *
      *            *---------------------------------------------------*
               10  srt-tip-arc            pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice archivio                                   *
      *            *---------------------------------------------------*
               10  srt-cod-arc            pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice dipendenza archivio                        *
      *            *---------------------------------------------------*
               10  srt-dpz-arc            pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Numero progressivo di registrazione               *
      *            *---------------------------------------------------*
               10  srt-num-prg            pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  srt-dat.
      *            *---------------------------------------------------*
      *            * Filler                                            *
      *            *---------------------------------------------------*
               10  srt-dat-fil.
                   15  filler             pic  x(01)                  .

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
                     "cnv"                                            .
      *        *-------------------------------------------------------*
      *        * Area gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-arg                  pic  x(03) value
                     "cnv"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "   "                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "zttadc"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "zttadc03"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "      Aggiornamento utenze clienti      "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "msegrt" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

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
      *    * Area di comunicazione per modulo                "mprint"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/p"                                  .

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
      *            *---------------------------------------------------*
      *            * Per routine let-sel-stp-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-let-sel-stp      pic  x(01)                  .
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
      *            * Si/No richieste per programma di esecuzione       *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-ric      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No funzionamento ciclico                       *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-cic      pic  x(01)                  .
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
      *    * Records files                                             *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [adc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfadc"                          .

      *    *===========================================================*
      *    * Work-area richieste per stampa                            *
      *    *-----------------------------------------------------------*
       01  rr.
           05  rr-tip-cnv                     pic  9(02)              .

      *    *===========================================================*
      *    * Work area per Determinazioni                              *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Per determinazione progressivo contatto               *
      *        *-------------------------------------------------------*
           05  w-det-prg-adc.
      *            *---------------------------------------------------*
      *            * Codice archivio                                   *
      *            *---------------------------------------------------*
               10  w-det-prg-adc-arc      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Progressivo                                       *
      *            *---------------------------------------------------*
               10  w-det-prg-adc-prg      pic  9(05)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo conversione                           *
      *        *-------------------------------------------------------*
           05  w-exp-tip-cnv.
               10  w-exp-tip-cnv-num      pic  9(02)       value 01   .
               10  w-exp-tip-cnv-lun      pic  9(02)       value 40   .
               10  w-exp-tip-cnv-tbl.
                   15  filler             pic  x(40) value
                          "Pulizia indirizzi mail obsoleti         "  .

      *    *===========================================================*
      *    * Work area di comodo                                       *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Contatori generici                                    *
      *        *-------------------------------------------------------*
           05  w-wrk-ctr-001              pic  9(06)                  .
           05  w-wrk-ctr-002              pic  9(03)                  .
           05  w-wrk-ctr-003              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per ridefinizione codice alfanumerico prodotto *
      *        *-------------------------------------------------------*
           05  w-wrk-alf-fnt              pic  x(03)                  .
           05  w-wrk-alf-pro              pic  x(14)                  .
           05  w-wrk-alf-pro-r  redefines
               w-wrk-alf-pro.
               10  w-wrk-alf-pro-fnt      pic  x(03)                  .
               10  w-wrk-alf-pro-cod      pic  9(04)                  .
               10  w-wrk-alf-pro-fil      pic  x(07)                  .
           05  w-wrk-alf-pro-r2 redefines
               w-wrk-alf-pro.
               10  w-wrk-alf-pro-fn2      pic  9(03)                  .
               10  w-wrk-alf-pro-co2      pic  9(04)                  .
               10  w-wrk-alf-pro-fi2      pic  x(07)                  .

      *    *===========================================================*
      *    * Work area per tabella valute appartenenti all'Euro        *
      *    *-----------------------------------------------------------*
       01  w-tab.
           05  w-tab-eml.
               10  w-tab-eml-tbl.
                   15  filler             pic  x(40) value
                    "jordanapuppa@hotmail.com                "        .
                   15  filler             pic  x(40) value
                    "lisa.gobbo@gmail.com                    "        .
                   15  filler             pic  x(40) value
                    "s.maggiolo@glaxipane.com                "        .
                   15  filler             pic  x(40) value
                    "miresi@castellobevilaqua.com            "        .
                   15  filler             pic  x(40) value
                    "ifioridititty@libero.it                 "        .
                   15  filler             pic  x(40) value
                    "bianchistefaniao@gmail.com              "        .
                   15  filler             pic  x(40) value
                    "franco.tappi@fastwebnet.it              "        .
                   15  filler             pic  x(40) value
                    "gibo841@libero.it                       "        .
                   15  filler             pic  x(40) value
                    "oltrelasiepe@simail.it                  "        .
                   15  filler             pic  x(40) value
                    "info@vicenzaverde.it                    "        .
                   15  filler             pic  x(40) value
                    "info@lerosedisharon.it                  "        .
                   15  filler             pic  x(40) value
                    "info@caffebedetti.191.it                "        .
                   15  filler             pic  x(40) value
                    "customer.service@alpegest.it            "        .
                   15  filler             pic  x(40) value
                    "frigonicola@libero.it                   "        .
                   15  filler             pic  x(40) value
                    "nonsolofiori@fioreriacogno.it           "        .
                   15  filler             pic  x(40) value
                    "info@pdpak.it                           "        .
                   15  filler             pic  x(40) value
                    "dellamico.ilaria@libero.it              "        .
                   15  filler             pic  x(40) value
                    "lucia.saglimbeni@gmail.com              "        .
                   15  filler             pic  x(40) value
                    "fioreriaai5petali@alice.it              "        .
                   15  filler             pic  x(40) value
                    "selcosnc@libero.it                      "        .
                   15  filler             pic  x(40) value
                    "mauro.cola@tin.it                       "        .
                   15  filler             pic  x(40) value
                    "angelone.gerarda@email.it               "        .
                   15  filler             pic  x(40) value
                    "gardenaitigli@virgilio.it               "        .
                   15  filler             pic  x(40) value
                    "info@fiocchistefano.it                  "        .
                   15  filler             pic  x(40) value
                    "giadasnc@live.it                        "        .
                   15  filler             pic  x(40) value
                    "info@robotino3.it                       "        .
                   15  filler             pic  x(40) value
                    "lgs@lgsste.com                          "        .
                   15  filler             pic  x(40) value
                    "feltrin.paola@libero.it                 "        .
                   15  filler             pic  x(40) value
                    "store.venturelli@libero.it              "        .
                   15  filler             pic  x(40) value
                    "silviadevoto@tiscali.it                 "        .
                   15  filler             pic  x(40) value
                    "green.matteucci@tin.it                  "        .
                   15  filler             pic  x(40) value
                    "mariannamiotti@miottispa.it             "        .
                   15  filler             pic  x(40) value
                    "fiori.co@liberto.it                     "        .
                   15  filler             pic  x(40) value
                    "studio@sarragioto.it                    "        .
                   15  filler             pic  x(40) value
                    "unfioreperte85@hotmail.it               "        .
                   15  filler             pic  x(40) value
                    "info@brendhouse.it                      "        .
                   15  filler             pic  x(40) value
                    "piero@florarici.191.it                  "        .
                   15  filler             pic  x(40) value
                    "santagrata@gmail.com                    "        .
                   15  filler             pic  x(40) value
                    "cecchipie@libero.it                     "        .
                   15  filler             pic  x(40) value
                    "antoluce@msn.com                        "        .
                   15  filler             pic  x(40) value
                    "angelaraso@allerasrl.com                "        .
                   15  filler             pic  x(40) value
                    "info@brentapelle.it                     "        .
                   15  filler             pic  x(40) value
                    "info@castpubblicita.it                  "        .
                   15  filler             pic  x(40) value
                    "scuolazoo@gmail.com                     "        .
                   15  filler             pic  x(40) value
                    "annistarzione@tascacavgiuseppe.it       "        .
                   15  filler             pic  x(40) value
                    "valevino@yahoo.it                       "        .
                   15  filler             pic  x(40) value
                    "naturalmente.dolce@virgilio.it          "        .
                   15  filler             pic  x(40) value
                    "simonechic@alice.it                     "        .
                   15  filler             pic  x(40) value
                    "aliona74@alice.it                       "        .
                   15  filler             pic  x(40) value
                    "info@hotelvillacostanza.com             "        .
                   15  filler             pic  x(40) value
                    "bottino-giuge@wanadoo.fr                "        .
                   15  filler             pic  x(40) value
                    "info@gerdenverdevivo.it                 "        .
                   15  filler             pic  x(40) value
                    "martacaremi@alice.it                    "        .
                   15  filler             pic  x(40) value
                    "info@labottegadelleideelecco.it         "        .
                   15  filler             pic  x(40) value
                    "info@cargo20.it                         "        .
                   15  filler             pic  x(40) value
                    "info@cioccolateriaserravalle.com        "        .
                   15  filler             pic  x(40) value
                    "pasticceriaroxibar@alice.it             "        .
                   15  filler             pic  x(40) value
                    "jean-francois20@wanadoo.fr              "        .
                   15  filler             pic  x(40) value
                    "mabelsurico@virgilio.it                 "        .
                   15  filler             pic  x(40) value
                    "info@gardenorchidea.it                  "        .
                   15  filler             pic  x(40) value
                    "gartenbaupozi@yahoo.it                  "        .
                   15  filler             pic  x(40) value
                    "stella@farmaciaallacastagnara.com       "        .
                   15  filler             pic  x(40) value
                    "pitagor@tiscali.it                      "        .
                   15  filler             pic  x(40) value
                    "lunic.selvazzano@live.it                "        .
                   15  filler             pic  x(40) value
                    "alessandra.roveccio@gruppoigd.it        "        .
                   15  filler             pic  x(40) value
                    "info@eugeniosimeoni.it                  "        .
                   15  filler             pic  x(40) value
                    "alanbord@alice.it                       "        .
                   15  filler             pic  x(40) value
                    "mariafra.mele@tiscalinet.it             "        .
                   15  filler             pic  x(40) value
                    "florideabs@libero.it                    "        .
                   15  filler             pic  x(40) value
                    "info@azzalinmarco.com                   "        .
                   15  filler             pic  x(40) value
                    "magiadeifiori@alice.it                  "        .
                   15  filler             pic  x(40) value
                    "elisabettanazzario@libero.it            "        .
                   15  filler             pic  x(40) value
                    "info@latradizione.com                   "        .
                   15  filler             pic  x(40) value
                    "mg@mgpadova.it                          "        .
                   15  filler             pic  x(40) value
                    "alfimar2009@live.it                     "        .
                   15  filler             pic  x(40) value
                    "info@fioreriadonatella.it               "        .
                   15  filler             pic  x(40) value
                    "donatella@teko.it                       "        .
                   15  filler             pic  x(40) value
                    "p.oliviero55@libero.it                  "        .
                   15  filler             pic  x(40) value
                    "gardencenteliloni@libero.it             "        .
                   15  filler             pic  x(40) value
                    "elianacenini@gmail.com                  "        .
                   15  filler             pic  x(40) value
                    "vetromecc@libero.it                     "        .
                   15  filler             pic  x(40) value
                    "infologgetto@libero.it                  "        .
                   15  filler             pic  x(40) value
                    "oltreilgiardino1@tiscali.it             "        .
                   15  filler             pic  x(40) value
                    "bepesrl@tin.it                          "        .
                   15  filler             pic  x(40) value
                    "misternielson@alice.it                  "        .
                   15  filler             pic  x(40) value
                    "info@siria.pd.it                        "        .
                   15  filler             pic  x(40) value
                    "giorgio-mazzoleni@alice.it              "        .
                   15  filler             pic  x(40) value
                    "sabine.armanini@libero.it               "        .
                   15  filler             pic  x(40) value
                    "interpost@interpost.191.it              "        .
                   15  filler             pic  x(40) value
                    "alberto.loreggian@gmail.com             "        .
                   15  filler             pic  x(40) value
                    "fotovideopaolo@gmail.com                "        .
                   15  filler             pic  x(40) value
                    "silvia.pietrobon@eniac.it               "        .
                   15  filler             pic  x(40) value
                    "esiflor@libero.it                       "        .
                   15  filler             pic  x(40) value
                    "miagreg@libero.it                       "        .
                   15  filler             pic  x(40) value
                    "ortofrutta5strade@alice.it              "        .
                   15  filler             pic  x(40) value
                    "alteagarden@alice.it                    "        .
                   15  filler             pic  x(40) value
                    "info@centrofiori.net                    "        .
                   15  filler             pic  x(40) value
                    "info@notecadaloris.it                   "        .
                   15  filler             pic  x(40) value
                    "barbara.stroppa@libero.it               "        .
                   15  filler             pic  x(40) value
                    "info@ilmondodianna.it                   "        .
                   15  filler             pic  x(40) value
                    "info@mapiverona.it                      "        .
                   15  filler             pic  x(40) value
                    "m.nizi@hotmail.it                       "        .
                   15  filler             pic  x(40) value
                    "interflora2413@virgilio.it              "        .
                   15  filler             pic  x(40) value
                    "info@edelveiss.it                       "        .
                   15  filler             pic  x(40) value
                    "info@fondazioneantonveneta.it           "        .
                   15  filler             pic  x(40) value
                    "sabinasolano@live.it                    "        .
                   15  filler             pic  x(40) value
                    "pviero@atena.net                        "        .
                   15  filler             pic  x(40) value
                    "laura.triches@openjob.it                "        .
                   15  filler             pic  x(40) value
                    "sami-@libero.it                         "        .
                   15  filler             pic  x(40) value
                    "marcogarbellotto@interfree.it           "        .
                   15  filler             pic  x(40) value
                    "brscia@nozzeedintorni.com               "        .
                   15  filler             pic  x(40) value
                    "contab@mcelettronica.it                 "        .
                   15  filler             pic  x(40) value
                    "i.or.go@hotmail.it                      "        .
                   15  filler             pic  x(40) value
                    "info@vitari.it                          "        .
                   15  filler             pic  x(40) value
                    "a.fashionflower@gmail.com               "        .
                   15  filler             pic  x(40) value
                    "giancarlogomiero@yahoo.it               "        .
                   15  filler             pic  x(40) value
                    "commercialealt@libero.it                "        .
                   15  filler             pic  x(40) value
                    "crimontanelli@live.it                   "        .
                   15  filler             pic  x(40) value
                    "martello.ie@gmail.com                   "        .
                   15  filler             pic  x(40) value
                    "info@flordametto.it                     "        .
                   15  filler             pic  x(40) value
                    "info@hotelgalassia.it                   "        .
                   15  filler             pic  x(40) value
                    "info@maximungroup.it                    "        .
                   15  filler             pic  x(40) value
                    "luisamussolin1952@libero.it             "        .
                   15  filler             pic  x(40) value
                    "arch.isabellaceccato@libero.it          "        .
                   15  filler             pic  x(40) value
                    "cepi@cepiflynn.it                       "        .
                   15  filler             pic  x(40) value
                    "info@hometissues.it                     "        .
                   15  filler             pic  x(40) value
                    "claudio@fioreriaroma.com                "        .
                   15  filler             pic  x(40) value
                    "info@bellucocostruzioni.com             "        .
                   15  filler             pic  x(40) value
                    "fatineefollettipoviglio@gmail.com       "        .
                   15  filler             pic  x(40) value
                    "maria.cuomo-86@libero.it                "        .
                   15  filler             pic  x(40) value
                    "info@chefparty.it                       "        .
                   15  filler             pic  x(40) value
                    "martina.cosmo@gmail.com                 "        .
                   15  filler             pic  x(40) value
                    "langolo.racconigi@gmail.com             "        .
                   15  filler             pic  x(40) value
                    "carab.giuseppe@hotmail.it               "        .
                   15  filler             pic  x(40) value
                    "elisir1982@tiscali.it                   "        .
                   15  filler             pic  x(40) value
                    "g.cagnin@libero.it                      "        .
                   15  filler             pic  x(40) value
                    "info@intimodeon.com                     "        .
                   15  filler             pic  x(40) value
                    "valli87@hotmail.com                     "        .
                   15  filler             pic  x(40) value
                    "info@magazzinipd.it                     "        .
                   15  filler             pic  x(40) value
                    "gazebo@fioriweb.it                      "        .
                   15  filler             pic  x(40) value
                    "a.difiore@inwind.it                     "        .
                   15  filler             pic  x(40) value
                    "jkebana@inwind.it                       "        .
                   15  filler             pic  x(40) value
                    "commerciale@ilgirasoleweb.com           "        .
                   15  filler             pic  x(40) value
                    "boscolo.matteo2008@libero.it            "        .
                   15  filler             pic  x(40) value
                    "gtricambiautosnc@virgilio.it            "        .
                   15  filler             pic  x(40) value
                    "fioridea_mn@alice.it                    "        .
                   15  filler             pic  x(40) value
                    "info@vinelia.com                        "        .
                   15  filler             pic  x(40) value
                    "kopykart@email.it                       "        .
                   15  filler             pic  x(40) value
                    "enotecavignocchi@tiscali.it             "        .
                   15  filler             pic  x(40) value
                    "fioriecolori@diesseweb.it               "        .
                   15  filler             pic  x(40) value
                    "info@alforum.it                         "        .
                   15  filler             pic  x(40) value
                    "carel@carel.com                         "        .
                   15  filler             pic  x(40) value
                    "jeierre.m@libero.it                     "        .
                   15  filler             pic  x(40) value
                    "info@babyland.it                        "        .
                   15  filler             pic  x(40) value
                    "simona@passerinisnc.191.it              "        .
                   15  filler             pic  x(40) value
                    "fotomarket.ufficio@libero.t             "        .
                   15  filler             pic  x(40) value
                    "info@parkhoteliris.com                  "        .
                   15  filler             pic  x(40) value
                    "silibera73@alice.it                     "        .
                   15  filler             pic  x(40) value
                    "giordani.sas@gmail.com                  "        .
                   15  filler             pic  x(40) value
                    "info@ottaviagrazia.it                   "        .
                   15  filler             pic  x(40) value
                    "favero@favero0a.191.it                  "        .
                   15  filler             pic  x(40) value
                    "acquaria79@fastwebnet.it                "        .
                   15  filler             pic  x(40) value
                    "info@fioreriavaleria.it                 "        .
                   15  filler             pic  x(40) value
                    "topgreenfeltre@gmail.com                "        .
                   15  filler             pic  x(40) value
                    "gontramicamillo@libero.it               "        .
                   15  filler             pic  x(40) value
                    "giulia_binotto@hotmail.com              "        .
                   15  filler             pic  x(40) value
                    "gardenprimavera@libero.it               "        .
                   15  filler             pic  x(40) value
                    "tonino.bonifazi@tiscali.it              "        .
                   15  filler             pic  x(40) value
                    "happycarto75@yahoo.it                   "        .
                   15  filler             pic  x(40) value
                    "euromarketleini@virgilio.it             "        .
                   15  filler             pic  x(40) value
                    "ilario37@mingardoilario.191.it          "        .
                   15  filler             pic  x(40) value
                    "info@cantinavalpantena.it               "        .
                   15  filler             pic  x(40) value
                    "ilbrucomela@ilbrucomela.net             "        .
                   15  filler             pic  x(40) value
                    "all.service@tiscalinet.it               "        .
                   15  filler             pic  x(40) value
                    "letybenve@gmail.com                     "        .
                   15  filler             pic  x(40) value
                    "info@hotelsantoli.com                   "        .
                   15  filler             pic  x(40) value
                    "dak.10@libero.it                        "        .
                   15  filler             pic  x(40) value
                    "nuvole.store@tiscali.it                 "        .
                   15  filler             pic  x(40) value
                    "cfpadova@tin.it                         "        .
                   15  filler             pic  x(40) value
                    "dinoflp@gmail.com                       "        .
                   15  filler             pic  x(40) value
                    "bertol1c@bertoldigaetano02.191.it       "        .
                   15  filler             pic  x(40) value
                    "andrea.montagna@virgilio.it             "        .
                   15  filler             pic  x(40) value
                    "giusevalerio@tiscali.it                 "        .
                   15  filler             pic  x(40) value
                    "molinari.cv@libero.it                   "        .
                   15  filler             pic  x(40) value
                    "silviamocisco@alice.it                  "        .
                   15  filler             pic  x(40) value
                    "guiviesi@tin.it                         "        .
                   15  filler             pic  x(40) value
                    "sonia.bergo1@virgilio.it                "        .
                   15  filler             pic  x(40) value
                    "paola.ronchetti@gmail.com               "        .
                   15  filler             pic  x(40) value
                    "noosheen@noosheens.com                  "        .
                   15  filler             pic  x(40) value
                    "info@mambotravel.it                     "        .
                   15  filler             pic  x(40) value
                    "padova.contabilita@farma3.it            "        .
                   15  filler             pic  x(40) value
                    "simone.bissacco@alice.it                "        .
                   15  filler             pic  x(40) value
                    "lidea_andora@virgilio.it                "        .
                   15  filler             pic  x(40) value
                    "mauriziodallafiore@gmail.com            "        .
                   15  filler             pic  x(40) value
                    "cccdtl70@hotmail.it                     "        .
                   15  filler             pic  x(40) value
                    "lidio1972@interfree.it                  "        .
                   15  filler             pic  x(40) value
                    "clara.manfrinato@libero.it              "        .
                   15  filler             pic  x(40) value
                    "stafania@novatecnos.191.it              "        .
                   15  filler             pic  x(40) value
                    "barboncinonero@gmail.com                "        .
                   15  filler             pic  x(40) value
                    "flowers@alice.it                        "        .
                   15  filler             pic  x(40) value
                    "nmaitre@libero.it                       "        .
                   15  filler             pic  x(40) value
                    "bazararredamenti@aol.it                 "        .
                   15  filler             pic  x(40) value
                    "direzione@centrolingue.com              "        .
                   15  filler             pic  x(40) value
                    "denisalessia@hotmail.it                 "        .
                   15  filler             pic  x(40) value
                    "info@atelierchatnoir.com                "        .
                   15  filler             pic  x(40) value
                    "pzanettin@libero.it                     "        .
                   15  filler             pic  x(40) value
                    "info@coop-primavera.it                  "        .
                   15  filler             pic  x(40) value
                    "pierangelo.66@hotmail.it                "        .
                   15  filler             pic  x(40) value
                    "profumeriarosanna@libero.it             "        .
                   15  filler             pic  x(40) value
                    "frammenti-c@libero.it                   "        .
                   15  filler             pic  x(40) value
                    "dafrom@libero.it                        "        .
                   15  filler             pic  x(40) value
                    "impresaconsolaro@alice.it               "        .
                   15  filler             pic  x(40) value
                    "info@fllicavicchia.com                  "        .
                   15  filler             pic  x(40) value
                    "gianna.sanmartino@gmail.com             "        .
                   15  filler             pic  x(40) value
                    "battistelli.valeria@libero.it           "        .
                   15  filler             pic  x(40) value
                    "michielotto@michielotto.com             "        .
                   15  filler             pic  x(40) value
                    "katybor@tin.ita                         "        .
                   15  filler             pic  x(40) value
                    "mondoverdegarden@libero.it              "        .
                   15  filler             pic  x(40) value
                    "info@artusogroup.it                     "        .
                   15  filler             pic  x(40) value
                    "eleonoradebei@vodafone.it               "        .
                   15  filler             pic  x(40) value
                    "olja-@libero.it                         "        .
                   15  filler             pic  x(40) value
                    "elmerenderoristorantepizzeria@gmail.com "        .
                   15  filler             pic  x(40) value
                    "laboutiquedelfiore@libero.it            "        .
                   15  filler             pic  x(40) value
                    "papaveri.azzurri@alice.it               "        .
                   15  filler             pic  x(40) value
                    "ermanno@laboratori.com                  "        .
                   15  filler             pic  x(40) value
                    "gabry29@hotmail.it                      "        .
                   15  filler             pic  x(40) value
                    "veho4@tecnocasa.it                      "        .
                   15  filler             pic  x(40) value
                    "edipiu@hotmail.com                      "        .
                   15  filler             pic  x(40) value
                    "lebouquet@libero.it                     "        .
                   15  filler             pic  x(40) value
                    "monica@tecnotradesrl.com                "        .
                   15  filler             pic  x(40) value
                    "zanzin@libero.it                        "        .
                   15  filler             pic  x(40) value
                    "info@boetzel.de                         "        .
                   15  filler             pic  x(40) value
                    "info@hotelsaturnia.it                   "        .
                   15  filler             pic  x(40) value
                    "roserosse.isolaliri@hotmail.com         "        .
                   15  filler             pic  x(40) value
                    "info@quintessential.it                  "        .
                   15  filler             pic  x(40) value
                    "elena-musclefit@libero.it               "        .
                   15  filler             pic  x(40) value
                    "armidobo@tiscali.it                     "        .
                   15  filler             pic  x(40) value
                    "christian.zolla@fastwebnet.it           "        .
                   15  filler             pic  x(40) value
                    "laura@alajmo.it                         "        .
                   15  filler             pic  x(40) value
                    "foto-fin@libero.it                      "        .
                   15  filler             pic  x(40) value
                    "ceolinalessandra@libero.it              "        .
                   15  filler             pic  x(40) value
                    "babive@hotmail.com                      "        .
                   15  filler             pic  x(40) value
                    "noviadiantonella@libero.it              "        .
                   15  filler             pic  x(40) value
                    "marketmarangoni@libero.it               "        .
                   15  filler             pic  x(40) value
                    "vaninsnc@interfree.it                   "        .
                   15  filler             pic  x(40) value
                    "rangonifiori@gmail.com                  "        .
                   15  filler             pic  x(40) value
                    "gardenpetaloblu@virgilio.it             "        .
                   15  filler             pic  x(40) value
                    "ofrosatimerigocodevigo@alice.it         "        .
                   15  filler             pic  x(40) value
                    "salonesonia@gmail.com                   "        .
                   15  filler             pic  x(40) value
                    "disdi@live.it                           "        .
                   15  filler             pic  x(40) value
                    "paneebonbon@alice.it                    "        .
                   15  filler             pic  x(40) value
                    "info@verdechiara.it                     "        .
                   15  filler             pic  x(40) value
                    "tizzi58@hotmail.it                      "        .
                   15  filler             pic  x(40) value
                    "ristorantedivino@gmail.com              "        .
                   15  filler             pic  x(40) value
                    "luisapesce@libero.it                    "        .
                   15  filler             pic  x(40) value
                    "bbaraldi25@hotmail.com                  "        .
                   15  filler             pic  x(40) value
                    "simona@biagisnc.it                      "        .
                   15  filler             pic  x(40) value
                    "valebt77@hotmail.com                    "        .
                   15  filler             pic  x(40) value
                    "info@acusticasistemi.it                 "        .
                   15  filler             pic  x(40) value
                    "a.barbato@ciapnet.it                    "        .
                   15  filler             pic  x(40) value
                    "katia@gardeniablu.net                   "        .
                   15  filler             pic  x(40) value
                    "bibi_d.p@katamail.it                    "        .
                   15  filler             pic  x(40) value
                    "lilianabellisario@libero.it             "        .
                   15  filler             pic  x(40) value
                    "elisabetta.nalon@bancamediolanum.it     "        .
                   15  filler             pic  x(40) value
                    "lemanitraicapellics@gmail.com           "        .
                   15  filler             pic  x(40) value
                    "ildesiderio_2008@libero.it              "        .
                   15  filler             pic  x(40) value
                    "cbozzolan@libero.it                     "        .
                   15  filler             pic  x(40) value
                    "st_2008@live.it                         "        .
                   15  filler             pic  x(40) value
                    "info@edendelfiore.it                    "        .
                   15  filler             pic  x(40) value
                    "info@pellizzarihousefarden.com          "        .
                   15  filler             pic  x(40) value
                    "lab@bitossi.com                         "        .
                   15  filler             pic  x(40) value
                    "officina@deifiori.net                   "        .
                   15  filler             pic  x(40) value
                    "donatellabottoni@alice.it               "        .
                   15  filler             pic  x(40) value
                    "boscaro.cri@libero.it                   "        .
                   15  filler             pic  x(40) value
                    "info@fabbrismario.com                   "        .
                   15  filler             pic  x(40) value
                    "talea55@live.it                         "        .
                   15  filler             pic  x(40) value
                    "valentina.brasile@libero.it             "        .
                   15  filler             pic  x(40) value
                    "alepiazzini@libero.it                   "        .
                   15  filler             pic  x(40) value
                    "info@fiorerialarosarossa.it             "        .
                   15  filler             pic  x(40) value
                    "e.pantaleoni@alice.it                   "        .
                   15  filler             pic  x(40) value
                    "info@hotelmignonpadova.it               "        .
                   15  filler             pic  x(40) value
                    "ipaalegnago@tin.it                      "        .
                   15  filler             pic  x(40) value
                    "veronese.maurizio@gmail.com             "        .
                   15  filler             pic  x(40) value
                    "info@tendarredopoletto.com              "        .
                   15  filler             pic  x(40) value
                    "info@studioassociato3a.com              "        .
                   15  filler             pic  x(40) value
                    "toffanello.g@libero.it                  "        .
                   15  filler             pic  x(40) value
                    "geafiori@libero.it                      "        .
                   15  filler             pic  x(40) value
                    "sandronava@pezzini.it                   "        .
                   15  filler             pic  x(40) value
                    "marisasalmaso@hotmail.it                "        .
                   15  filler             pic  x(40) value
                    "info@verde-mania.it                     "        .
                   15  filler             pic  x(40) value
                    "info@turriniferramenta.it               "        .
                   15  filler             pic  x(40) value
                    "info@happyline.it                       "        .
                   15  filler             pic  x(40) value
                    "robertob@freepass.it                    "        .
                   15  filler             pic  x(40) value
                    "info@kitesrl.it                         "        .
                   15  filler             pic  x(40) value
                    "info@alex-srl.it                        "        .
                   15  filler             pic  x(40) value
                    "amministarzione@deika.it                "        .
                   15  filler             pic  x(40) value
                    "info@studioboaretto.com                 "        .
                   15  filler             pic  x(40) value
                    "evacecchi@hotmail.com                   "        .
                   15  filler             pic  x(40) value
                    "info@giannifurlan.com                   "        .
                   15  filler             pic  x(40) value
                    "giulianademarchi@tiscali.it             "        .
                   15  filler             pic  x(40) value
                    "info@pasticceriapicchio.com             "        .
                   15  filler             pic  x(40) value
                    "info@ilbellodellacasa.com               "        .
                   15  filler             pic  x(40) value
                    "fioreriabrigadoi@virgilio.it            "        .
                   15  filler             pic  x(40) value
                    "info@graficheeden.it                    "        .
                   15  filler             pic  x(40) value
                    "info@aiguidovie.191.it                  "        .
                   15  filler             pic  x(40) value
                    "bagnoli18@interfree.it                  "        .
                   15  filler             pic  x(40) value
                    "info@brainstormsol.com                  "        .
                   15  filler             pic  x(40) value
                    "info@italmobildivr.it                   "        .
                   15  filler             pic  x(40) value
                    "gabriella@sanmartino.it                 "        .
                   15  filler             pic  x(40) value
                    "luglioguarino193@gmail.com              "        .
                   15  filler             pic  x(40) value
                    "maison@maisonaltamare.it                "        .
                   15  filler             pic  x(40) value
                    "jessica@harmattan.it                    "        .
                   15  filler             pic  x(40) value
                    "nellaviz@libero.it                      "        .
                   15  filler             pic  x(40) value
                    "mellonitiziana@libero.it                "        .
                   15  filler             pic  x(40) value
                    "bon.zurigo@libero.it                    "        .
                   15  filler             pic  x(40) value
                    "gabriele.righetto@gmail.com             "        .
                   15  filler             pic  x(40) value
                    "cantine.bernabei@in.wind.it             "        .
                   15  filler             pic  x(40) value
                    "info@sartosrl.it                        "        .
                   15  filler             pic  x(40) value
                    "alexcorsini@libero.it                   "        .
                   15  filler             pic  x(40) value
                    "abigon@teletu.it                        "        .
                   15  filler             pic  x(40) value
                    "floricoltura.montin@libero.it           "        .
                   15  filler             pic  x(40) value
                    "info@gruppomelina.it                    "        .
                   15  filler             pic  x(40) value
                    "info@ristoangeli.it                     "        .
                   15  filler             pic  x(40) value
                    "fiorerialenny@email.it                  "        .
                   15  filler             pic  x(40) value
                    "info@colombomaurizio.it                 "        .
                   15  filler             pic  x(40) value
                    "info@madesani1913.it                    "        .
                   15  filler             pic  x(40) value
                    "maripangallo@yahoo.it                   "        .
                   15  filler             pic  x(40) value
                    "info@lugah.eu                           "        .
                   15  filler             pic  x(40) value
                    "lineaco@tin.it                          "        .
                   15  filler             pic  x(40) value
                    "agatavitale1@alice.it                   "        .
                   15  filler             pic  x(40) value
                    "giovatta@libero.it                      "        .
                   15  filler             pic  x(40) value
                    "corsichiara2009@libero.it               "        .
                   15  filler             pic  x(40) value
                    "cuore.store@gmail.com                   "        .
                   15  filler             pic  x(40) value
                    "chiaran6@gmail.com                      "        .
                   15  filler             pic  x(40) value
                    "giovanna.marchionni@tele2.it            "        .
                   15  filler             pic  x(40) value
                    "marzialchemilla@alice.it                "        .
                   15  filler             pic  x(40) value
                    "ilgiardinodigaribaldi@legalmail.it      "        .
                   15  filler             pic  x(40) value
                    "ombretta.gaffi@alice.it                 "        .
                   15  filler             pic  x(40) value
                    "moreno.schianta@alice.it                "        .
                   15  filler             pic  x(40) value
                    "javajob@tiscali.it                      "        .
                   15  filler             pic  x(40) value
                    "cfc@cfc_italia.com                      "        .
                   15  filler             pic  x(40) value
                    "info@bormioliauto.it                    "        .
                   15  filler             pic  x(40) value
                    "tuttiifiori@alice.it                    "        .
                   15  filler             pic  x(40) value
                    "dama.save@libero.it                     "        .
                   15  filler             pic  x(40) value
                    "info@stefanodellavedova.it              "        .
                   15  filler             pic  x(40) value
                    "info@vivaifleming.it                    "        .
                   15  filler             pic  x(40) value
                    "alessandro6tiso@gmail.com               "        .
                   15  filler             pic  x(40) value
                    "flli.daminato@tiscali.it                "        .
                   15  filler             pic  x(40) value
                    "tridents@tridentviaggi.191.it           "        .
                   15  filler             pic  x(40) value
                    "tommaso220595@katmail.com               "        .
                   15  filler             pic  x(40) value
                    "info@ristorantepontecorvo.com           "        .
                   15  filler             pic  x(40) value
                    "info@casadelgiardino2.it                "        .
                   15  filler             pic  x(40) value
                    "punto.auto2@virgilio.it                 "        .
                   15  filler             pic  x(40) value
                    "lica81@tiscali.it                       "        .
                   15  filler             pic  x(40) value
                    "luisav1982@libero.it                    "        .
                   15  filler             pic  x(40) value
                    "info@cavalliluce.it                     "        .
                   15  filler             pic  x(40) value
                    "info@millemigliatv.it                   "        .
                   15  filler             pic  x(40) value
                    "xeka1976@yahoo.it                       "        .
                   15  filler             pic  x(40) value
                    "raffaella@esperidi.com                  "        .
                   15  filler             pic  x(40) value
                    "fioreriabetty@hotmail.it                "        .
                   15  filler             pic  x(40) value
                    "clipsomoda@fastwebnet.it                "        .
                   15  filler             pic  x(40) value
                    "booking@parkvenezia.com                 "        .
                   15  filler             pic  x(40) value
                    "ilaria.longo@yahoo.it                   "        .
                   15  filler             pic  x(40) value
                    "info@30529volkswagengroup.it            "        .
                   15  filler             pic  x(40) value
                    "info@cadeau.it                          "        .
                   15  filler             pic  x(40) value
                    "nicoletti@geat.it                       "        .
                   15  filler             pic  x(40) value
                    "rosalba.andretti@libero.it              "        .
                   15  filler             pic  x(40) value
                    "chiara.gibin@yahoo.it                   "        .
                   15  filler             pic  x(40) value
                    "info@ristoranteamicimiei.it             "        .
                   15  filler             pic  x(40) value
                    "a.canal@vidorisalotti.it                "        .
                   15  filler             pic  x(40) value
                    "info@fantasyflowers.it                  "        .
                   15  filler             pic  x(40) value
                    "info@casadelbambino.191.it              "        .
                   15  filler             pic  x(40) value
                    "gnagnarellasnc@virgilio.it              "        .
                   15  filler             pic  x(40) value
                    "fiorerianina@gmail.com                  "        .
                   15  filler             pic  x(40) value
                    "francesca_noce@yahoo.it                 "        .
                   15  filler             pic  x(40) value
                    "silv.sca@libero.it                      "        .
                   15  filler             pic  x(40) value
                    "nick-gra@libero.it                      "        .
                   15  filler             pic  x(40) value
                    "tomac@telematicaitalia.it               "        .
                   15  filler             pic  x(40) value
                    "treerre1@hotmail.it                     "        .
                   15  filler             pic  x(40) value
                    "ironmilo@alice.it                       "        .
                   15  filler             pic  x(40) value
                    "giardinofiorito@libero.it               "        .
                   15  filler             pic  x(40) value
                    "robertorussi@alice.it                   "        .
                   15  filler             pic  x(40) value
                    "info@dasavio.it                         "        .
                   15  filler             pic  x(40) value
                    "brescia@eurflor.it                      "        .
                   15  filler             pic  x(40) value
                    "denis.coi@euroimpianticoi.it            "        .
                   15  filler             pic  x(40) value
                    "follow_linda@hotmail.it                 "        .
                   15  filler             pic  x(40) value
                    "tessutitendaggi@yahoo.it                "        .
                   15  filler             pic  x(40) value
                    "aironemare@tiscali.it                   "        .
                   15  filler             pic  x(40) value
                    "drmaurizio@eurocostruzioni-spa.com      "        .
                   15  filler             pic  x(40) value
                    "info@mvparrucchieri.com                 "        .
                   15  filler             pic  x(40) value
                    "serenaedanilo@hotmail.it                "        .
                   15  filler             pic  x(40) value
                    "veronicaia@virgilio.it                  "        .
                   15  filler             pic  x(40) value
                    "claudio.basso@hotmail.it                "        .
                   15  filler             pic  x(40) value
                    "andre.garn@hotmail.it                   "        .
                   15  filler             pic  x(40) value
                    "casaloffia@casaloffia.it                "        .
                   15  filler             pic  x(40) value
                    "carlo.bigarella@fastwebnet.it           "        .
                   15  filler             pic  x(40) value
                    "romina.gastaldon@libero.it              "        .
                   15  filler             pic  x(40) value
                    "k.denada@email.it                       "        .
                   15  filler             pic  x(40) value
                    "noidavoi@virgilio.it                    "        .
                   15  filler             pic  x(40) value
                    "fiorie0a@fioriesementisala.191.it       "        .
                   15  filler             pic  x(40) value
                    "anna.parolisi@virgilio.it               "        .
                   15  filler             pic  x(40) value
                    "andrea.giaretta@libero.it               "        .
                   15  filler             pic  x(40) value
                    "ideafioreria@libero.it                  "        .
                   15  filler             pic  x(40) value
                    "lunagrafica@alice.it                    "        .
                   15  filler             pic  x(40) value
                    "callegaro.matteo@gmail.com              "        .
                   15  filler             pic  x(40) value
                    "alessandro@conceptstudios.it            "        .
                   15  filler             pic  x(40) value
                    "baccini@acciaodolce.it                  "        .
                   15  filler             pic  x(40) value
                    "malmusi@improntaverde.net               "        .
                   15  filler             pic  x(40) value
                    "samarta76@libero.it                     "        .
                   15  filler             pic  x(40) value
                    "laura.venturi@iscombo.it                "        .
                   15  filler             pic  x(40) value
                    "stefanobaschieri@virgilio.it            "        .
                   15  filler             pic  x(40) value
                    "lucia.miotto@fastwebnet.it              "        .
                   15  filler             pic  x(40) value
                    "info@tecarflor.it                       "        .
                   15  filler             pic  x(40) value
                    "ericasnc@live.it                        "        .
                   15  filler             pic  x(40) value
                    "santinaroversi@tiscali.it               "        .
                   15  filler             pic  x(40) value
                    "750dorsoduro@libero.it                  "        .
                   15  filler             pic  x(40) value
                    "ilgiardinodeidesideri@live.it           "        .
                   15  filler             pic  x(40) value
                    "slavemark79@live.it                     "        .
                   15  filler             pic  x(40) value
                    "maevy.impianti@hotmail.it               "        .
                   15  filler             pic  x(40) value
                    "fioreriazaggia@libero.it                "        .
                   15  filler             pic  x(40) value
                    "info@officinesalin.it                   "        .
                   15  filler             pic  x(40) value
                    "ilmsterassaio@ilmaterassaio.com         "        .
                   15  filler             pic  x(40) value
                    "minuziosa@libero.it                     "        .
                   15  filler             pic  x(40) value
                    "roberta@lacortedeifiori.it              "        .
                   15  filler             pic  x(40) value
                    "massnc@email.it                         "        .
                   15  filler             pic  x(40) value
                    "bardelcorsocolletorto@gmail.com         "        .
                   15  filler             pic  x(40) value
                    "vetrineilaria@live.it                   "        .
                   15  filler             pic  x(40) value
                    "monia.lucidi@alice.it                   "        .
                   15  filler             pic  x(40) value
                    "fioreriaarcobaleno@gmail.com            "        .
                   15  filler             pic  x(40) value
                    "chiaraciabattari@yahoo.it               "        .
                   15  filler             pic  x(40) value
                    "cristiana_zortea@diesel.com             "        .
                   15  filler             pic  x(40) value
                    "carla@optikrom.it                       "        .
                   15  filler             pic  x(40) value
                    "segato.impianti@excite.it               "        .
                   15  filler             pic  x(40) value
                    "info@consorzio-alisei.it                "        .
                   15  filler             pic  x(40) value
                    "claudiaborsa@gmail.com                  "        .
                   15  filler             pic  x(40) value
                    "succi.enea@libero.it                    "        .
                   15  filler             pic  x(40) value
                    "pieroloap@hotmail.it                    "        .
                   15  filler             pic  x(40) value
                    "direzione@hotelallatorre.it             "        .
                   15  filler             pic  x(40) value
                    "lallina1479@hotmail.it                  "        .
                   15  filler             pic  x(40) value
                    "s.zago@systemsrl.it                     "        .
                   15  filler             pic  x(40) value
                    "info@bmcontact.it                       "        .
                   15  filler             pic  x(40) value
                    "cuoredisardegna@alice.it                "        .
                   15  filler             pic  x(40) value
                    "pianetafiore@tiscali.it                 "        .
                   15  filler             pic  x(40) value
                    "trilli.fiori@gmail.com                  "        .
                   15  filler             pic  x(40) value
                    "info@supergrisi.it                      "        .
                   15  filler             pic  x(40) value
                    "fuori-rotta@libero.it                   "        .
                   15  filler             pic  x(40) value
                    "pinella.martella@gmsil.com              "        .
                   15  filler             pic  x(40) value
                    "info@apropositodifiori.com              "        .
                   15  filler             pic  x(40) value
                    "arieri@libero.it                        "        .
                   15  filler             pic  x(40) value
                    "marcello.graziella@live.it              "        .
                   15  filler             pic  x(40) value
                    "batistelli.valeria@libero.it            "        .
                   15  filler             pic  x(40) value
                    "info@hotelroyalpadova.com               "        .
                   15  filler             pic  x(40) value
                    "baki.kiss@libero.it                     "        .
                   15  filler             pic  x(40) value
                    "ambient@lascalamania.com                "        .
                   15  filler             pic  x(40) value
                    "manu2121@hotmail.it                     "        .
                   15  filler             pic  x(40) value
                    "luc@arch1.191.it                        "        .
                   15  filler             pic  x(40) value
                    "tanja.lorenzon@lorenzonsrl.it           "        .
                   15  filler             pic  x(40) value
                    "trieste@nozzeedintorni.com              "        .
                   15  filler             pic  x(40) value
                    "elettrocasasoatto@libero.it             "        .
                   15  filler             pic  x(40) value
                    "info@merlo-figli.191.it                 "        .
                   15  filler             pic  x(40) value
                    "doriana.dimatteo@tin.it                 "        .
                   15  filler             pic  x(40) value
                    "a.cadore@areadati.com                   "        .
                   15  filler             pic  x(40) value
                    "calisto@ivg.it                          "        .
                   15  filler             pic  x(40) value
                    "ppaola599@gmail.com                     "        .
                   15  filler             pic  x(40) value
                    "stefania.bloise@tint.it                 "        .
                   15  filler             pic  x(40) value
                    "enrico.busacchi@tin.it                  "        .
                   15  filler             pic  x(40) value
                    "info@feltrin.net                        "        .
                   15  filler             pic  x(40) value
                    "alicedifalco@libero.it                  "        .
                   15  filler             pic  x(40) value
                    "info@baggioedesordi.com                 "        .
                   15  filler             pic  x(40) value
                    "info@rivoaltus.it                       "        .
                   15  filler             pic  x(40) value
                    "info@artigliodeldiavolo.it              "        .
                   15  filler             pic  x(40) value
                    "emanuela@saicmec.it                     "        .
                   15  filler             pic  x(40) value
                    "info@enricoorchidee.it                  "        .
                   15  filler             pic  x(40) value
                    "deborah@gfeventplanner.com              "        .
                   15  filler             pic  x(40) value
                    "roberta.sambugaro@van-loon.com          "        .
                   15  filler             pic  x(40) value
                    "valentina@eventi.cc                     "        .
                   15  filler             pic  x(40) value
                    "barbara.bacelle@gmail.com               "        .
                   15  filler             pic  x(40) value
                    "giusy.longhitano@rarisitaormina.it      "        .
                   15  filler             pic  x(40) value
                    "enrica@chocolatcdg.it                   "        .
                   15  filler             pic  x(40) value
                    "ilgigante.ed@tiscali.it                 "        .
                   15  filler             pic  x(40) value
                    "arale.83@libero.it                      "        .
                   15  filler             pic  x(40) value
                    "cinzia.rolle@posta.it                   "        .
                   15  filler             pic  x(40) value
                    "michelabarzoi@libero.it                 "        .
                   15  filler             pic  x(40) value
                    "librazoo@alice.it                       "        .
                   15  filler             pic  x(40) value
                    "info@coopairone.it                      "        .
                   15  filler             pic  x(40) value
                    "e.mail-ikebana76@tiscali.it             "        .
                   15  filler             pic  x(40) value
                    "info@carrozzeriacaonada.it              "        .
                   15  filler             pic  x(40) value
                    "luca@lucaefriends.it                    "        .
                   15  filler             pic  x(40) value
                    "serenafash@libero.it                    "        .
                   15  filler             pic  x(40) value
                    "info@arteinfiore.it                     "        .
                   15  filler             pic  x(40) value
                    "segreteria@anfasschio.it                "        .
                   15  filler             pic  x(40) value
                    "info@artesidea.it                       "        .
                   15  filler             pic  x(40) value
                    "alessiasrl1@gmail.com                   "        .
                   15  filler             pic  x(40) value
                    "vale.flower@libero.it                   "        .
                   15  filler             pic  x(40) value
                    "piazzettadeifiori@tiscali.it            "        .
                   15  filler             pic  x(40) value
                    "mariniantonio@libero.it                 "        .
                   15  filler             pic  x(40) value
                    "info@af365.it                           "        .
                   15  filler             pic  x(40) value
                    "verdeexpo@coop-pgfrassati.org           "        .
                   15  filler             pic  x(40) value
                    "gianeifu@fastwebnet.it                  "        .
                   15  filler             pic  x(40) value
                    "giannettmax@alice.it                    "        .
                   15  filler             pic  x(40) value
                    "gardenregina@alice.it                   "        .
                   15  filler             pic  x(40) value
                    "valentinoroncador@tin.it                "        .
                   15  filler             pic  x(40) value
                    "info@clublegiarette.it                  "        .
                   15  filler             pic  x(40) value
                    "lecasematte@libero.it                   "        .
                   15  filler             pic  x(40) value
                    "piwi@hotmail.it                         "        .
                   15  filler             pic  x(40) value
                    "info@giordaninox.it                     "        .
                   15  filler             pic  x(40) value
                    "centrogiardinobetti@alice.it            "        .
                   15  filler             pic  x(40) value
                    "iacopo.giraldo@gmail.com                "        .
                   15  filler             pic  x(40) value
                    "info@centroverde.com                    "        .
                   15  filler             pic  x(40) value
                    "info.ceramichelia@gmail.com             "        .
                   15  filler             pic  x(40) value
                    "info@sunesteticsilvia.it                "        .
                   15  filler             pic  x(40) value
                    "fioreriaperviero@tiscali.it             "        .
                   15  filler             pic  x(40) value
                    "enrica@freecolor.it                     "        .
                   15  filler             pic  x(40) value
                    "ferroniste@virgilio.it                  "        .
                   15  filler             pic  x(40) value
                    "misuratenda@alice.it                    "        .
                   15  filler             pic  x(40) value
                    "fiordaliso@libero.it                    "        .
                   15  filler             pic  x(40) value
                    "info@generalgreen.it                    "        .
                   15  filler             pic  x(40) value
                    "info@studiogiona.com                    "        .
                   15  filler             pic  x(40) value
                    "info@vitaverdegiardini.it               "        .
                   15  filler             pic  x(40) value
                    "info@paccagnellasnc.191.it              "        .
                   15  filler             pic  x(40) value
                    "fioreriailmelograno@jahoo.it            "        .
                   15  filler             pic  x(40) value
                    "flowers.power@hotmail.it                "        .
                   15  filler             pic  x(40) value
                    "alfeo.vendramin@alice.it                "        .
                   15  filler             pic  x(40) value
                    "info@thee42.com                         "        .
                   15  filler             pic  x(40) value
                    "bottegaorafacortina@teletu.it           "        .
                   15  filler             pic  x(40) value
                    "damacaffe@libero.it                     "        .
                   15  filler             pic  x(40) value
                    "wkgiannaromito@libero.it                "        .
                   15  filler             pic  x(40) value
                    "davide@fontanaconsulting.it             "        .
                   15  filler             pic  x(40) value
                    "parafarmaciaruffin@mail.it              "        .
                   15  filler             pic  x(40) value
                    "katiusciaflower@yahoo.it                "        .
                   15  filler             pic  x(40) value
                    "gioiotticamarilu@tiscali.it             "        .
                   15  filler             pic  x(40) value
                    "biscarodenise@libero.it                 "        .
                   15  filler             pic  x(40) value
                    "alba3ett@libero.it                      "        .
                   15  filler             pic  x(40) value
                    "cristianamovy@libero.it                 "        .
                   15  filler             pic  x(40) value
                    "sfamu@tahoo.com                         "        .
                   15  filler             pic  x(40) value
                    "fioreriamarilla@gmail.com               "        .
                   15  filler             pic  x(40) value
                    "brigna2003@libero.it                    "        .
                   15  filler             pic  x(40) value
                    "silvia91@hotmail.it                     "        .
                   15  filler             pic  x(40) value
                    "info@svi.it                             "        .
                   15  filler             pic  x(40) value
                    "romanoeu1@studio-romano.191.it          "        .
                   15  filler             pic  x(40) value
                    "zussadaniela@gmail.com                  "        .
                   15  filler             pic  x(40) value
                    "elena@giolo.it                          "        .
                   15  filler             pic  x(40) value
                    "sevens07@seven-seas.191.it              "        .
                   15  filler             pic  x(40) value
                    "info@ladogaressaflowers.com             "        .
                   15  filler             pic  x(40) value
                    "info@claudiomeneghin.it                 "        .
                   15  filler             pic  x(40) value
                    "follonicalorena@libero.it               "        .
                   15  filler             pic  x(40) value
                    "info@pasticceriadeluca.it               "        .
                   15  filler             pic  x(40) value
                    "noncisono75@libero.it                   "        .
                   15  filler             pic  x(40) value
                    "amministrazione@impiantimcm.it          "        .
                   15  filler             pic  x(40) value
                    "info@publistar.191.it                   "        .
                   15  filler             pic  x(40) value
                    "salonemiki@gmail.com                    "        .
                   15  filler             pic  x(40) value
                    "fioriecapricci@hotmail.it               "        .
                   15  filler             pic  x(40) value
                    "info@florafantastica.it                 "        .
                   15  filler             pic  x(40) value
                    "anna.masala@gmail.com                   "        .
                   15  filler             pic  x(40) value
                    "info@vivaipandolfo.it                   "        .
                   15  filler             pic  x(40) value
                    "info@hotelbonardi.it                    "        .
                   15  filler             pic  x(40) value
                    "paola@vichadv.it                        "        .
                   15  filler             pic  x(40) value
                    "cmasotto@rana.it                        "        .
                   15  filler             pic  x(40) value
                    "lerobevece@libero.it                    "        .
                   15  filler             pic  x(40) value
                    "rasimarcello@libero.it                  "        .
                   15  filler             pic  x(40) value
                    "rosavivaldi@tim.it                      "        .
                   15  filler             pic  x(40) value
                    "vigiservice.snc@legalmail.it            "        .
                   15  filler             pic  x(40) value
                    "flowerscream@gmail.com                  "        .
                   15  filler             pic  x(40) value
                    "info@larosarossacollection.com          "        .
                   15  filler             pic  x(40) value
                    "chakracconciature@libero.it             "        .
                   15  filler             pic  x(40) value
                    "magiav02@erbmagiaverde.191.it           "        .
                   15  filler             pic  x(40) value
                    "studiovillalta@libero.it                "        .
                   15  filler             pic  x(40) value
                    "nfrancois@comptoir-du-fleuriste.com     "        .
                   15  filler             pic  x(40) value
                    "marty.z87@libero.it                     "        .
                   15  filler             pic  x(40) value
                    "sondraferri@libero.it                   "        .
                   15  filler             pic  x(40) value
                    "info@pec.cis-montebello.it              "        .
                   15  filler             pic  x(40) value
                    "monica.boardo@alice.it                  "        .
                   15  filler             pic  x(40) value
                    "pepo88@live.it                          "        .
                   15  filler             pic  x(40) value
                    "sarazamp@jahoo.it                       "        .
                   15  filler             pic  x(40) value
                    "ennio.trippetta@gmail.com               "        .
                   15  filler             pic  x(40) value
                    "spetinfia@hotmail.com                   "        .
                   15  filler             pic  x(40) value
                    "info@importlegnosrl.com                 "        .
                   15  filler             pic  x(40) value
                    "patriziagalvan@gmail.com                "        .
                   15  filler             pic  x(40) value
                    "novellosaverio@libero.it                "        .
                   15  filler             pic  x(40) value
                    "bo_enireteoil&nonoil@eni.it             "        .
                   15  filler             pic  x(40) value
                    "giancarlo.vecchi@hotmail.com            "        .
                   15  filler             pic  x(40) value
                    "m.zonzin@tecnocasa.net                  "        .
                   15  filler             pic  x(40) value
                    "gaia@celebritylab.com                   "        .
                   15  filler             pic  x(40) value
                    "massimo-secchi@libero.it                "        .
                   15  filler             pic  x(40) value
                    "nicoli.luca&tin.it                      "        .
                   15  filler             pic  x(40) value
                    "f.llimelis@tiscali.it                   "        .
                   15  filler             pic  x(40) value
                    "lagostinelli@luiss.it                   "        .
                   15  filler             pic  x(40) value
                    "jessica.jessica2010@libero.it           "        .
                   15  filler             pic  x(40) value
                    "giosue@nativoverona.it                  "        .
               10  w-tab-eml-tbr redefines
                   w-tab-eml-tbl.
                   15  w-tab-eml-ele occurs 589
                                 indexed  by   w-tab-eml-inx          .
                       20  w-tab-eml-eml  pic  x(40)                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

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
      *              * Open files                                      *
      *              *-------------------------------------------------*
           perform   rou-opn-fls-000      thru rou-opn-fls-999        .
           if        w-cnt-rou-opn-fls    not  = spaces
                     go to main-800.
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-800.
      *              *-------------------------------------------------*
      *              * Preparazione tipo funzionamento programma       *
      *              *-------------------------------------------------*
           perform   pre-tip-fun-000      thru pre-tip-fun-999        .
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
                     go to main-800.
      *                  *---------------------------------------------*
      *                  * Se uscita per 'N'                           *
      *                  *---------------------------------------------*
           if        w-cnt-acc-ric-sel    =    "N"
                     go to main-250.
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
                     go to main-800.
       main-450.
      *              *-------------------------------------------------*
      *              * Esecuzione in foreground                        *
      *              *-------------------------------------------------*
           perform   exe-pgm-frg-000      thru exe-pgm-frg-999        .
      *              *-------------------------------------------------*
      *              * Test se tipo esecuzione ciclico                 *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S" or
                     w-cnt-fun-snx-cic    not  = "S"
                     go to main-800
           else      go to main-250.
       main-800.
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
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-pgm-frg-300.
      *              *-------------------------------------------------*
      *              * Esecuzione del programma                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura parametri di selezione stampa       *
      *                  *---------------------------------------------*
           perform   let-sel-stp-000      thru let-sel-stp-999        .
           if        w-cnt-let-sel-stp    not  = spaces
                     go to exe-pgm-frg-999.
      *                  *---------------------------------------------*
      *                  * Esecuzione eventuale sort preliminare       *
      *                  *---------------------------------------------*
           perform   exe-rou-srt-000      thru exe-rou-srt-999        .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se sort eseguito       *
      *                  *---------------------------------------------*
           if        w-cnt-exe-rou-srt    =    spaces
                     go to exe-pgm-frg-400
           else      go to exe-pgm-frg-500.
       exe-pgm-frg-400.
      *                  *---------------------------------------------*
      *                  * Se sort non eseguito                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ciclo di report-program                 *
      *                      *-----------------------------------------*
           perform   prn-rou-pri-000      thru prn-rou-pri-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     exe-pgm-frg-600.
       exe-pgm-frg-500.
      *                  *---------------------------------------------*
      *                  * Se sort eseguito                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     exe-pgm-frg-600.
       exe-pgm-frg-600.
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo stampa                 *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mprint"                         .
       exe-pgm-frg-900.
      *                  *---------------------------------------------*
      *                  * Visual. eventuali errori di esecuzione      *
      *                  *---------------------------------------------*
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
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Preparazione tipo funzionamento programma                 *
      *    *-----------------------------------------------------------*
       pre-tip-fun-000.
      *              *-------------------------------------------------*
      *              * Si/No richieste ad utente                       *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-ric      .
      *              *-------------------------------------------------*
      *              * Si/No funzionamento ciclico stampa              *
      *              *-------------------------------------------------*
           move      "N"                  to   w-cnt-fun-snx-cic      .
      *              *-------------------------------------------------*
      *              * Si/No richiesta di selezione stampa             *
      *              *-------------------------------------------------*
           move      "N"                  to   w-cnt-fun-snx-stp      .
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
      *              * [adc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [adc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
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
       acc-ric-sel-150.
      *                  *---------------------------------------------*
      *                  * Tipo conversione                            *
      *                  *---------------------------------------------*
           perform   acc-tip-cnv-000      thru acc-tip-cnv-999        .
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
      *              * Tipo conversione                                *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo conversione           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Tipo conversione           *
      *    *-----------------------------------------------------------*
       acc-tip-cnv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-cnv-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-cnv-lun    to   v-car                  .
           move      w-exp-tip-cnv-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-cnv-tbl    to   v-txt                  .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-tip-cnv           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-cnv-999.
       acc-tip-cnv-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tip-cnv             .
       acc-tip-cnv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-tip-cnv           =    zero
                     go to acc-tip-cnv-100.
       acc-tip-cnv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-cnv-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-cnv-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-cnv-100.
       acc-tip-cnv-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
           move      zero                 to   rr-tip-cnv             .
       nor-ric-sel-999.
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
      *              *-------------------------------------------------*
      *              * Tipo conversione                                *
      *              *-------------------------------------------------*
           if        rr-tip-cnv           =    zero
                     move  01             to   rr-tip-cnv             .
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Preparazione parametri per selezione stampa               *
      *    *-----------------------------------------------------------*
       pre-prm-stp-000.
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
       pre-prm-stp-999.
           exit.

      *    *===========================================================*
      *    * Routine di sort preliminare                               *
      *    *-----------------------------------------------------------*
       exe-rou-srt-000.
      *              *-------------------------------------------------*
      *              * Flag di sort eseguito a : Si'                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-exe-rou-srt      .
      *              *-------------------------------------------------*
      *              * Esecuzione sort                                 *
      *              *-------------------------------------------------*
           sort      srt                  on   ascending srt-key
                     input  procedure     is   stp-srt-inp-000
                                          thru stp-srt-inp-999
                     output procedure     is   prn-rou-pri-000
                                          thru prn-rou-pri-999        .
       exe-rou-srt-999.
           exit.

      *    *===========================================================*
      *    * Input procedure per sort                                  *
      *    *-----------------------------------------------------------*
       stp-srt-inp-000.
      *              *-------------------------------------------------*
      *              * Ciclo per [adc]                                 *
      *              *-------------------------------------------------*
           perform   stp-srt-inp-adc-000  thru stp-srt-inp-adc-999    .
       stp-srt-inp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-srt-inp-999.
       stp-srt-inp-999.
           exit.

      *    *===========================================================*
      *    * Input procedure per sort                                  *
      *    *                                                           *
      *    * Ciclo per [adc]                                           *
      *    *-----------------------------------------------------------*
       stp-srt-inp-adc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-prg-adc-prg      .
       stp-srt-inp-adc-100.
      *              *-------------------------------------------------*
      *              * Start su archivio [adc]                         *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "ARCCON    "         to   f-key                  .
           move      02                   to   rf-adc-tip-arc         .
           move      zero                 to   rf-adc-cod-arc         .
           move      spaces               to   rf-adc-dpz-arc         .
           move      zero                 to   rf-adc-num-prg         .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-srt-inp-adc-900.
       stp-srt-inp-adc-200.
      *              *-------------------------------------------------*
      *              * Read-next su archivio [adc]                     *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
      *                  *---------------------------------------------*
      *                  * Se at end : uscita                          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-srt-inp-adc-900.
       stp-srt-inp-adc-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo archivio                       *
      *                  *---------------------------------------------*
           if        rf-adc-tip-arc       not  = 02
                     go to stp-srt-inp-adc-900.
       stp-srt-inp-adc-400.
      *              *-------------------------------------------------*
      *              * Selezioni sul record [adc]                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione indice                      *
      *                  *---------------------------------------------*
           set       w-tab-eml-inx        to   1                      .
      *                  *---------------------------------------------*
      *                  * Ricerca                                     *
      *                  *---------------------------------------------*
           search    w-tab-eml-ele
                     when    w-tab-eml-eml
                            (w-tab-eml-inx)
                                          =    rf-adc-num-con
                     go to stp-srt-inp-adc-600.
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     stp-srt-inp-adc-200.
       stp-srt-inp-adc-600.
      *              *-------------------------------------------------*
      *              * Composizione record di sort                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           move      spaces               to   srt-rec                .
      *                  *---------------------------------------------*
      *                  * Chiave per il sort                          *
      *                  *---------------------------------------------*
           move      rf-adc-tip-arc       to   srt-tip-arc            .
           move      rf-adc-cod-arc       to   srt-cod-arc            .
           move      rf-adc-dpz-arc       to   srt-dpz-arc            .
           move      rf-adc-num-prg       to   srt-num-prg            .
       stp-srt-inp-adc-700.
      *                  *---------------------------------------------*
      *                  * Rilascio record al sort                     *
      *                  *---------------------------------------------*
           release   srt-rec                                          .
       stp-srt-inp-adc-800.
      *                  *---------------------------------------------*
      *                  * Indicatore di programma in esecuzione       *
      *                  *---------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Riciclo a record [adc] successivo           *
      *                  *---------------------------------------------*
           go to     stp-srt-inp-adc-200.
       stp-srt-inp-adc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-srt-inp-adc-999.
       stp-srt-inp-adc-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Start iniziale                     *
      *    *-----------------------------------------------------------*
       prn-str-ini-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore record aggiornati     *
      *              *-------------------------------------------------*
           move      zero                 to   w-wrk-ctr-001          .
       prn-str-ini-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Messaggio per nessuna registraz.   *
      *    *-----------------------------------------------------------*
       prn-nes-ela-000.
           move      "WR"                 to   m-ope                  .
           move      "Nessun elemento entro i limiti assegnati !"
                                          to   m-msg                  .
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
      *              *-------------------------------------------------*
      *              * Lettura sequenziale archivio sortato [srt]      *
      *              *-------------------------------------------------*
           return    srt    at end
                            move  "#"     to   w-cnt-prn-flg-sub
                            go to prn-let-seq-999.
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
      *              *-------------------------------------------------*
      *              * Editing contatore                               *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      w-wrk-ctr-001        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Messaggio di fine esecuzione                    *
      *              *-------------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Cancellate nr. "
                                delimited by   size
                     p-edt      delimited by   spaces
                     " mail"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
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
      *              *-------------------------------------------------*
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       prn-liv-det-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
      *              *-------------------------------------------------*
      *              * Lettura record                                  *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "ARCCON"             to   f-key                  .
           move      srt-tip-arc          to   rf-adc-tip-arc         .
           move      srt-cod-arc          to   rf-adc-cod-arc         .
           move      srt-dpz-arc          to   rf-adc-dpz-arc         .
           move      srt-num-prg          to   rf-adc-num-prg         .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato : uscita              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to prn-liv-det-999.
       prn-liv-det-300.
      *              *-------------------------------------------------*
      *              * Cancellazione record                            *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
      *              *-------------------------------------------------*
      *              * Aggiornamento contatore                         *
      *              *-------------------------------------------------*
           add       1                    to   w-wrk-ctr-001          .
       prn-liv-det-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-liv-det-999.
       prn-liv-det-999.
           exit.

      *    *===========================================================*
      *    * Intestazione foglio                                       *
      *    *-----------------------------------------------------------*
       int-pag-sta-000.
       int-pag-sta-999.
           exit.

      *    *===========================================================*
      *    * Determinazione del codice numerico prodotto               *
      *    *-----------------------------------------------------------*
       det-prg-adc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valore di uscita                *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-prg-adc-prg      .
       det-prg-adc-100.
      *              *-------------------------------------------------*
      *              * Start su archivio [adc]                         *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "ARCCON    "         to   f-key                  .
           move      01                   to   rf-adc-tip-arc         .
           move      w-det-prg-adc-arc    to   rf-adc-cod-arc         .
           move      spaces               to   rf-adc-dpz-arc         .
           move      zero                 to   rf-adc-num-prg         .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-prg-adc-900.
       det-prg-adc-200.
      *              *-------------------------------------------------*
      *              * Read-next su archivio [adc]                     *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
      *                  *---------------------------------------------*
      *                  * Test se fine file                           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-prg-adc-900.
       det-prg-adc-300.
      *              *-------------------------------------------------*
      *              * Test max su archivio [adc]                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo archivio                       *
      *                  *---------------------------------------------*
           if        rf-adc-tip-arc       not  = 01
                     go to det-prg-adc-900.
      *                  *---------------------------------------------*
      *                  * Test su codice archivio                     *
      *                  *---------------------------------------------*
           if        rf-adc-cod-arc       not  = w-det-prg-adc-arc
                     go to det-prg-adc-900.
       det-prg-adc-400.
      *              *-------------------------------------------------*
      *              * Incremento progressivo                          *
      *              *-------------------------------------------------*
           add       1                    to   w-det-prg-adc-prg      .
       det-prg-adc-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     det-prg-adc-200.
       det-prg-adc-900.
      *              *-------------------------------------------------*
      *              * Incremento progressivo                          *
      *              *-------------------------------------------------*
           add       1                    to   w-det-prg-adc-prg      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-prg-adc-999.
       det-prg-adc-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

