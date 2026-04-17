       Identification Division.
       Program-Id.                                 pxpg0005           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    swd                 *
      *                        Area gestionale:    xpg                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 01/01/01    *
      *                       Ultima revisione:    NdK del 10/11/05    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Scelta codice dipendenza                    *
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
      *    * Area di interfaccia per sottoprogramma         "pazi000d" *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/pazi000d.pgl"                   .

      ******************************************************************
       Procedure Division.
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Cancellazione moduli di i-o obsoleti.           *
      *              *                                                 *
      *              * Il richiamo di questa funzione e' indispensabi- *
      *              * le per impedire che due richiami consecutivi da *
      *              * menu' del presente programma, inframmezzati da  *
      *              * un cambio codice azienda, lascino aperto il mo- *
      *              * dulo di i-o del file [tbl], facendo sbagliare   *
      *              * la funzione di determinazione dei codici dipen- *
      *              * denze per l'azienda, di seguito richiamata.     *
      *              *-------------------------------------------------*
           move      "IX"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Accettazione codice dipendenza                  *
      *              *-------------------------------------------------*
           perform   acc-cod-dpz-000      thru acc-cod-dpz-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione moduli di i-o obsoleti.           *
      *              *                                                 *
      *              * Il richiamo di questa funzione e' indispensabi- *
      *              * le per impedire che due richiami consecutivi da *
      *              * menu' del presente programma, inframmezzati da  *
      *              * un cambio codice azienda, lascino aperto il mo- *
      *              * dulo di i-o del file [tbl], facendo sbagliare   *
      *              * la funzione di determinazione dei codici dipen- *
      *              * denze per l'azienda, di seguito richiamata.     *
      *              *-------------------------------------------------*
           move      "IX"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       main-999.
           exit      program                                          .

      *    *===========================================================*
      *    * Accettazione codice dipendenza                            *
      *    *-----------------------------------------------------------*
       acc-cod-dpz-000.
      *              *-------------------------------------------------*
      *              * Determinazione codici dipendenze per l'azienda  *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "DA"                 to   w-dpz-tip-ope          .
           move      s-ter                to   w-dpz-ide-ter          .
           move      s-ute                to   w-dpz-ide-ute          .
           move      s-azi                to   w-dpz-ide-azi          .
           move      s-sap                to   w-dpz-ide-sap          .
           move      s-arg                to   w-dpz-ide-arg          .
           move      s-set                to   w-dpz-ide-set          .
           move      s-fas                to   w-dpz-ide-fas          .
           move      spaces               to   w-dpz-ide-des          .
           move      s-pro                to   w-dpz-ide-pro          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
           cancel    "pgm/azi/prg/obj/pazi000d"                       .
      *              *-------------------------------------------------*
      *              * Se esiste una sola dipendenza : oltre           *
      *              *-------------------------------------------------*
           if        w-dpz-ctr-dpz        =    1
                     go to acc-cod-dpz-200.
      *              *-------------------------------------------------*
      *              * Se zero dipendenze : errore ed uscita           *
      *              *-------------------------------------------------*
           if        w-dpz-ctr-dpz        >    zero
                     go to acc-cod-dpz-200.
           move      "EN"                 to   w-dpz-tip-ope          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
           cancel    "pgm/azi/prg/obj/pazi000d"                       .
           go to     acc-cod-dpz-999.
       acc-cod-dpz-200.
      *              *-------------------------------------------------*
      *              * Selezione codice dipendenza per il programma,   *
      *              * proponendo come default il codice dipendenza    *
      *              * precentemente in uso presso la segreteria       *
      *              *-------------------------------------------------*
           move      "D<"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "FD"                 to   w-dpz-tip-ope          .
           move      zero                 to   w-dpz-cod-prg          .
           move      s-num                to   w-dpz-cod-dpz          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
           cancel    "pgm/azi/prg/obj/pazi000d"                       .
       acc-cod-dpz-999.
           exit.

