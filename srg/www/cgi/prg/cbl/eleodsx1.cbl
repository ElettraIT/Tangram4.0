       Identification Division.
       Program-Id.                                 eleodsx1           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    www                 *
      *                        Area gestionale:    cgi                 *
      *                                Settore:    ele                 *
      *                                   Fase:    eleods              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 26/06/23    *
      *                       Ultima revisione:    NdK del 09/10/26    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Spunta OK su OSR: flag NBX(3)               *
      *                                                                *
      *                    ELETTRA (VERSIONE NUOVA)                    *
      *                                                                *
      *================================================================*


      ******************************************************************
       Environment Division.
      ******************************************************************

      *================================================================*
       Configuration Section.
      *================================================================*

       Source-Computer.     w-i-p-NdK-PD.
       Object-Computer.     w-i-p-NdK-PD.

       Special-Names.       Decimal-Point is comma.

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
      *    * Area di comunicazione per modulo                "mopsys"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/o"                                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [osr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfosr".
      *        *-------------------------------------------------------*
      *        * [zrm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzrm".

      *    *===========================================================*
      *    * Work-area routine di trattamento variabile POST           *
      *    *                                                           *
      *    * ELETTRA                                                   *
      *    *-----------------------------------------------------------*
           copy      "ele/cgi/prg/cpy/elecgi00.cpw"                   .

      *    *===========================================================*
      *    * Area di comodo                                            *
      *    *-----------------------------------------------------------*
       01  w-exe.
      *        *-------------------------------------------------------*
      *        * Campi di comodo                                       *
      *        *-------------------------------------------------------*
           05  w-exe-mod-ope              pic x(01).
           05  w-exe-tip-ope              pic x(03).
           05  w-exe-cod-rsm              pic x(03).
           05  w-exe-prt-ods              pic x(11).
           05  w-exe-prg-ods              pic x(05).
           05  w-exe-flg-nbx              pic x(01).
           05  w-exe-rsm-cnv              pic 9(03).
           05  w-exe-prt-cnv              pic 9(11).
           05  w-exe-prg-cnv              pic 9(05).
           05  w-exe-ok                   pic x(01).
           05  w-exe-opn-osr              pic x(01).
           05  w-exe-opn-zrm              pic x(01).
           05  w-exe-lck-osr              pic x(01).
           05  w-exe-prm-err              pic x(01).
           05  w-exe-error                pic x(24).
           05  w-exe-json                 pic x(128).
           05  w-exe-num-alf              pic x(11).
           05  w-exe-num-lun              pic 9(02).
           05  w-exe-num-inx              pic 9(02).
           05  w-exe-num-err              pic x(01).
      *        *-------------------------------------------------------*
      *        * Comodi per regolarizzazioni                           *
      *        *-------------------------------------------------------*
           05  w-exe-prm-vx1              pic  x(20)                  .
           05  w-exe-prm-vx2              pic  x(20)                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .


      ******************************************************************
       Procedure Division                                             .
      ******************************************************************

      *================================================================*
      * Main                                                           *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      spaces to w-exe.
           move      zero to w-exe-rsm-cnv w-exe-prt-cnv
                             w-exe-prg-cnv w-exe-num-lun
                             w-exe-num-inx.
           move      "N" to w-exe-ok w-exe-opn-osr w-exe-opn-zrm
                            w-exe-lck-osr.
      *    * Default del solo CGI dedicato: compatibilita' client NBX.
           move      "R" to w-exe-mod-ope.
           move      "NBX" to w-exe-tip-ope.
           move      "BAD_REQUEST" to w-exe-error.
           perform   ext-prm-000 thru ext-prm-999.
           if        w-exe-prm-err = "S"
                     go to main-800.
           perform   val-prm-000 thru val-prm-999.
           if        w-exe-prm-err = "S"
                     go to main-800.
           perform   opn-fls-000 thru opn-fls-999.
           if        w-exe-opn-osr not = "S" or
                     w-exe-opn-zrm not = "S"
                     go to main-800.
           perform   val-rsm-000 thru val-rsm-999.
           if        w-exe-prm-err = "S"
                     go to main-800.
           perform   upd-ok-000 thru upd-ok-999.
       main-800.
           perform   cls-fls-000 thru cls-fls-999.
           perform   emi-json-000 thru emi-json-999.
       main-999.
           exit      program.

      *    *===========================================================*
      *    * Estrazione parametri                                      *
      *    *-----------------------------------------------------------*
       ext-prm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione parametri                       *
      *              *-------------------------------------------------*
           move      "NO"                 to   w-cgi-tip-ope          .
           move      06                   to   w-cgi-str-num          .
           perform   ope-prm-inp-000      thru ope-prm-inp-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile di environment          *
      *              *-------------------------------------------------*
           move      "I2"                 to   o-ope                  .
           move      "POST"               to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *              *-------------------------------------------------*
      *              * Estrazione parametri                            *
      *              *-------------------------------------------------*
           move      o-pst                to   w-cgi-str-var          .
           perform   cgi-str-ext-000      thru cgi-str-ext-999        .
      *              *-------------------------------------------------*
      *              * Assegnazione componenti                         *
      *              *-------------------------------------------------*
           move      "EX"                 to   w-cgi-tip-ope          .
           perform   ope-prm-inp-000      thru ope-prm-inp-999        .
       ext-prm-999.
           exit.

      *    *===========================================================*
      *    * Estrazione parametri                                      *
      *    *                                                           *
      *    * Subroutine di assegnazione del valore in base al nome del *
      *    * campo in input                                            *
      *    *-----------------------------------------------------------*
       ext-prm-ass-000.
           move w-all-str-cat (2) to w-all-str-alf.
           perform all-str-lun-000 thru all-str-lun-999.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del nome elemento        *
      *              *-------------------------------------------------*
           if w-all-str-cat (1) = "rsp_doc" or "tip_ope"
              if w-all-str-lun > 3
                 move "S" to w-exe-prm-err.
           if w-all-str-cat (1) = "prt_ods" or "num_prt"
              if w-all-str-lun > 11
                 move "S" to w-exe-prm-err.
           if w-all-str-cat (1) = "prg_ods" or "num_prg"
              if w-all-str-lun > 5
                 move "S" to w-exe-prm-err.
           if w-all-str-cat (1) = "mod_ope"
              if w-all-str-cat (2) not = "R"
                 move "S" to w-exe-prm-err.

           if w-all-str-cat (1) = "mod_ope"
              move w-all-str-cat (2) to w-exe-mod-ope
           else if w-all-str-cat (1) = "tip_ope"
              move w-all-str-cat (2) to w-exe-tip-ope
           else if w-all-str-cat (1) = "rsp_doc"
              move w-all-str-cat (2) to w-exe-cod-rsm
           else if w-all-str-cat (1) = "prt_ods" or "num_prt"
              move w-all-str-cat (2) to w-exe-prt-ods
           else if w-all-str-cat (1) = "prg_ods" or "num_prg"
              move w-all-str-cat (2) to w-exe-prg-ods
           else if w-all-str-cat (1) = "flg_nbx"
              if w-all-str-cat (2) = "S" or "N"
                 move w-all-str-cat (2) to w-exe-flg-nbx
              else
                 move "S" to w-exe-prm-err
           else if w-all-str-cat (1) = "nbx"
              if w-all-str-cat (2) = "#"
                 move "S" to w-exe-flg-nbx
              else if w-all-str-cat (2) = spaces
                 move "N" to w-exe-flg-nbx
              else
                 move "S" to w-exe-prm-err
           else if w-all-str-cat (1) = "idx_nbx"
              if w-all-str-cat (2) not = "3"
                 move "S" to w-exe-prm-err.
       ext-prm-ass-900.
           go to ext-prm-ass-999.
       ext-prm-ass-999.
           exit.

      *    *===========================================================*
      *    * Controllo modalita', operazione, chiave e spunta.
      *    *-----------------------------------------------------------*
       val-prm-000.
           if        w-exe-mod-ope not = "R" or
                     w-exe-tip-ope not = "NBX"
                     move "S" to w-exe-prm-err
                     go to val-prm-999.
           if        w-exe-flg-nbx not = "S" and
                     w-exe-flg-nbx not = "N"
                     move "S" to w-exe-prm-err
                     go to val-prm-999.
           move      spaces to w-exe-num-alf.
           move      w-exe-cod-rsm to w-exe-num-alf.
           move      03 to w-exe-num-lun.
           perform   val-num-000 thru val-num-999.
           if        w-exe-num-err = "S"
                     move "S" to w-exe-prm-err
                     go to val-prm-999.
           move      "CV" to p-ope.
           move      03 to p-car.
           move      w-exe-cod-rsm to p-alf.
           call      "swd/mod/prg/obj/mprint" using p.
           move      p-num to w-exe-rsm-cnv.
           move      w-exe-prt-ods to w-exe-num-alf.
           move      11 to w-exe-num-lun.
           perform   val-num-000 thru val-num-999.
           if        w-exe-num-err = "S"
                     move "S" to w-exe-prm-err
                     go to val-prm-999.
           move      "CV" to p-ope.
           move      11 to p-car.
           move      w-exe-prt-ods to p-alf.
           call      "swd/mod/prg/obj/mprint" using p.
           move      p-num to w-exe-prt-cnv.
           move      spaces to w-exe-num-alf.
           move      w-exe-prg-ods to w-exe-num-alf.
           move      05 to w-exe-num-lun.
           perform   val-num-000 thru val-num-999.
           if        w-exe-num-err = "S"
                     move "S" to w-exe-prm-err
                     go to val-prm-999.
           move      "CV" to p-ope.
           move      05 to p-car.
           move      w-exe-prg-ods to p-alf.
           call      "swd/mod/prg/obj/mprint" using p.
           move      p-num to w-exe-prg-cnv.
           if        w-exe-rsm-cnv = zero or
                     w-exe-prt-cnv = zero or w-exe-prg-cnv = zero
                     move "S" to w-exe-prm-err.
       val-prm-999.
           exit.

      *    *===========================================================*
      *    * Controllo dei caratteri numerici prima della conversione.
      *    *-----------------------------------------------------------*
       val-num-000.
           move      spaces to w-exe-num-err.
           move      zero to w-exe-num-inx.
       val-num-100.
           add       1 to w-exe-num-inx.
           if        w-exe-num-inx > w-exe-num-lun
                     go to val-num-999.
           if w-exe-num-alf (w-exe-num-inx:1) = space
              if w-exe-num-inx = 1
                 move "S" to w-exe-num-err
                 go to val-num-999
              else
                 go to val-num-200.
           if        w-exe-num-alf (w-exe-num-inx:1) < "0" or
                     w-exe-num-alf (w-exe-num-inx:1) > "9"
                     move "S" to w-exe-num-err
                     go to val-num-999.
           go to     val-num-100.
       val-num-200.
      *    * Dopo la prima cifra accetta soltanto spazi finali.
           add 1 to w-exe-num-inx.
           if w-exe-num-inx > w-exe-num-lun
              go to val-num-999.
           if w-exe-num-alf (w-exe-num-inx:1) not = space
              move "S" to w-exe-num-err
              go to val-num-999.
           go to val-num-200.
       val-num-999.
           exit.

      *    *===========================================================*
      *    * Apertura OSR e ZRM.
      *    *-----------------------------------------------------------*
       opn-fls-000.
           move      "OPEN_FAILED" to w-exe-error.
           move      "OP" to f-ope.
           move      "pgm/ods/fls/ioc/obj/iofosr" to s-pat.
           call      "swd/mod/prg/obj/mfiltp" using s.
           call      s-pat using f rf-osr.
           if        f-sts not = e-not-err
                     go to opn-fls-999.
           move      "S" to w-exe-opn-osr.
           move      "OP" to f-ope.
           move      "pgm/mag/fls/ioc/obj/iofzrm" to s-pat.
           call      "swd/mod/prg/obj/mfiltp" using s.
           call      s-pat using f rf-zrm.
           if        f-sts = e-not-err
                     move "S" to w-exe-opn-zrm.
       opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Verifica operatore iniziale su ZRM / CODRSP.
      *    *-----------------------------------------------------------*
       val-rsm-000.
           move      "RK" to f-ope.
           move      "CODRSP" to f-key.
           move      w-exe-rsm-cnv to rf-zrm-cod-rsp.
           move      "pgm/mag/fls/ioc/obj/iofzrm" to s-pat.
           call      "swd/mod/prg/obj/mfiltp" using s.
           call      s-pat using f rf-zrm.
           if        f-sts not = e-not-err
                     move "S" to w-exe-prm-err
                     move "OPERATOR_INVALID" to w-exe-error.
       val-rsm-999.
           exit.

      *    *===========================================================*
      *    * Blocco riga OSR esatta, aggiornamento del solo NBX(3).
      *    *-----------------------------------------------------------*
       upd-ok-000.
           move      "ROW_UNAVAILABLE" to w-exe-error.
           move      "GK" to f-ope.
           move      "NUMPRT    " to f-key.
           move      w-exe-prt-cnv to rf-osr-num-prt.
           move      w-exe-prg-cnv to rf-osr-num-prg.
           move      "pgm/ods/fls/ioc/obj/iofosr" to s-pat.
           call      "swd/mod/prg/obj/mfiltp" using s.
           call      s-pat using f rf-osr.
           if        f-sts not = e-not-err
                     go to upd-ok-999.
           move      "S" to w-exe-lck-osr.
           if        rf-osr-num-prt not = w-exe-prt-cnv or
                     rf-osr-num-prg not = w-exe-prg-cnv
                     move "ROW_MISMATCH" to w-exe-error
                     go to upd-ok-800.
           if        w-exe-flg-nbx = "S"
                     move "#" to rf-osr-flg-nbx (3)
           else      move spaces to rf-osr-flg-nbx (3).
           move      "WRITE_FAILED" to w-exe-error.
           move      "UP" to f-ope.
           move      "pgm/ods/fls/ioc/obj/iofosr" to s-pat.
           call      "swd/mod/prg/obj/mfiltp" using s.
           call      s-pat using f rf-osr.
           if        f-sts = e-not-err
                     move "S" to w-exe-ok
                     move spaces to w-exe-error.
       upd-ok-800.
           move      "RL" to f-ope.
           move      "pgm/ods/fls/ioc/obj/iofosr" to s-pat.
           call      "swd/mod/prg/obj/mfiltp" using s.
           call      s-pat using f rf-osr.
           if        f-sts not = e-not-err
                     move "RELEASE_FAILED" to w-exe-error
                     go to upd-ok-999.
           move      "N" to w-exe-lck-osr.
       upd-ok-999.
           exit.

      *    *===========================================================*
      *    * Chiusura archivi e segnalazione errori.
      *    *-----------------------------------------------------------*
       cls-fls-000.
           if        w-exe-opn-osr not = "S"
                     go to cls-fls-200.
           move      "CL" to f-ope.
           move      "pgm/ods/fls/ioc/obj/iofosr" to s-pat.
           call      "swd/mod/prg/obj/mfiltp" using s.
           call      s-pat using f rf-osr.
           if        f-sts not = e-not-err
                     move "CLOSE_FAILED" to w-exe-error.
       cls-fls-200.
           if        w-exe-opn-zrm not = "S"
                     go to cls-fls-999.
           move      "CL" to f-ope.
           move      "pgm/mag/fls/ioc/obj/iofzrm" to s-pat.
           call      "swd/mod/prg/obj/mfiltp" using s.
           call      s-pat using f rf-zrm.
           if        f-sts not = e-not-err
                     move "CLOSE_FAILED" to w-exe-error.
       cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Conferma UP; errori successivi come warning.
      *    *-----------------------------------------------------------*
       emi-json-000.
      *    * Solo body JSON, come eleodsx0 nel launcher applicativo.
           if        w-exe-ok = "S" and w-exe-error = spaces
                     display '{"ok":true}'
                     go to emi-json-999.
           move      spaces to w-exe-json.
           if        w-exe-ok = "S"
                     go to emi-json-100.
           string    '{"ok":false,"error":"'
                     delimited by size
                     w-exe-error delimited by spaces
                     '"}' delimited by size
                     into w-exe-json.
           display   w-exe-json.
           go to     emi-json-999.
       emi-json-100.
           string    '{"ok":true,"warning":"'
                     delimited by size
                     w-exe-error delimited by spaces
                     '"}' delimited by size
                     into w-exe-json.
           display   w-exe-json.
       emi-json-999.
           exit.

      *    *===========================================================*
      *    * Subroutines di trattamento variabile POST                 *
      *    *                                                           *
      *    * ELETTRA                                                   *
      *    *-----------------------------------------------------------*
           copy      "ele/cgi/prg/cpy/elecgi00.cps"                   .

      *    *===========================================================*
      *    * Routine di lettura archivio [zub]                         *
      *    *-----------------------------------------------------------*
           copy "swd/std/prg/cpy/wallstr0.cps".
