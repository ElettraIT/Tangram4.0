       Identification Division.
       Program-Id.                                 calc               .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:                        *
      *                                   Fase:    cliins              *
      *                    ------------------------------------------- *
      *                       Versione attuale:    001 del 01/01/00    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Inserimento clienti da Internet             *
      *                                                                *
      *================================================================*

      ******************************************************************
       Environment Division.
      ******************************************************************
       Configuration Section.
       special-names.   crt status is event.
       data division.

      *================================================================*
       Working-Storage Section.
      *================================================================*
       77  ind                pic 9.
       77  ind1               pic 9.
       77  i                  pic 99.
       77  mult               pic 9(15)      comp-3.
       77  multd              pic 9(15)      comp-3.
       77  comodo             pic s9(13)v9(5) sign trailing separate.
       77  z                  pic ------,---,---,--9.
       77  zv1                pic ----,---,---,--9.9.
       77  zv2                pic ---,---,---,--9.99.
       77  zv3                pic ------,---,--9.999.
       77  zv4                pic -----,---,--9.9999.
       77  zv5                pic ----,---,--9.99999.
       77  edidata            pic 99/99/99 blank when zero.
       77  ncif               pic 99  value 0.
       77  k                  pic s99 value 0.
       77  k1                 pic 99  value 0.
       
       77                     pic 9 value 0.
           88  fl-tv          value 1 false 0.
       
       77                     pic 9 value 0.
           88  fl-vi          value 1 false 0.
       
       77  event              pic 9(3).
           88 e_ret           value 13.
           88 key_up          value 52.
           88 key_down        value 53.
           88 key_right       value 54.
       
       77                     pic 9 value  0.
           88 line_total      value 1 false 0.
       
       78  max_array          value 50.
       
       01  array_scroll.
           03                 occurs max_array.
              05 number_c     pic x(22).
              05 chr_sign     pic x.
                 88 chr_subtract value "-".
       01  array_work         pic x(1150).

       77  calc               pic 9(3) comp-x.
       77  paper              pic 9(3) comp-x.
       77  calc_sub           pic 9(3) comp-x.
       01  terminal_abilities.
           03  pic x(15).
           03  pic x.
               88  has_color                   value "Y".
           03  pic x(9).

       01  elem_array         pic 99 comp-x value 0.
       01  start_list         pic 99 comp-x value 0.
       01  end_list           pic 99 comp-x value 0.
       
       01  ndec               pic 9 value 0.
           88 decimal0        value 0.
           88 decimal1        value 1.
           88 decimal2        value 2.
           88 decimal3        value 3.
           88 decimal4        value 4.
           88 decimal5        value 5.
       
       01  max_dec            pic 9.
       
       01  importo            pic s9(13)v9(5) sign trailing separate.
       01  totale             pic s9(13)v9(5).
       01  operando           pic s9(13)v9(5).
       01  counter            pic 9(5) comp-1 value 0.
       
       01  pic 9              value 0.
           88 reset_counter   value 1 false 0.
       
       01  type_op            pic 9.
           88 mult_op         value 1.
           88 div_op          value 2.
       
       01  x                  pic x.
           88 key_ok          value "0" thru "9"
                                             ","
                                             "+"
                                             "-"
                                             "*"
                                             "/"
                                             "="
                                             "C"
                                             "E"
                                             "."                      .
           88 cifra           value "0" thru "9".
           88 clear           value "C".
           88 ce              value "E".
           88 virg            value "," ".".
           88 no_value        value " ".
           88 end_program     value "F" "T".
           88 tras_total      value "T".
           88 help            value "H".
           88 cnt             value "N".
           88 op_add_sub      value "+" "-".
           88 op_add          value "+".
           88 op_subtract     value "-".
           88 op_multiply     value "*".
           88 op_divide       value "/".
           88 op_total        value "=".
       
       01     redefines x.
           03 c               pic 9.
       
       01  vettore.
           03 v               pic 9 occurs 12.
       
       01  disp.
           03 d1              pic x(3)  value spaces.
           03 d2              pic x(18) value spaces.
           03 d3              pic x     value spaces.
       
       01  ora.
           03 hh              pic 99.
           03 mm              pic 99.
           03 ss              pic 99.
           03 cc              pic 99.
       
       01  ora-d.
           03 hhe             pic 99.
           03                 pic x  value ":".
           03 mme             pic 99.
           03                 pic x  value ":".
           03 sse             pic 99.
       
       01  data_c             pic 9(6).
       01           redefines data_c.
           03 gg_c            pic 99.
           03 mm_c            pic 99.
           03 aa_c            pic 99.
       
       01  data_r             pic 9(6).
       01           redefines data_r.
           03 aa_r            pic 99.
           03 mm_r            pic 99.
           03 gg_r            pic 99.
       
       01  pua                pic x(10).
       01  pua1               pic x(10).
       01  pua2               pic x(10).
       
      *================================================================*
       linkage section.
      *================================================================*
       01 current-field.
          03 cf          pic x  occurs 1 to 200 times
                                          depending on field_size.
       01 field_size     pic s9(4) comp-1.
      *================================================================*
       Procedure Division using current_field field_size.
      *================================================================*
       main.
       
           set environment "decimal-point" to ",".
       
           perform set-keyboard.
           perform set-color.
           perform mas thru mas-ex.
           perform cal thru cal-ex.
       
       main-ex.
           perform reset-keyboard.
       
       main-ex-ex.   goback.
       
       cal.
           move 0 to z  move z to d2.
           display disp line 2 position 4 high
       
           perform ini-var thru ini-var-ex.
       
           perform until end_program
       
              move spaces to x
              accept x at 0426 upper prompt auto color calc
                     off before    time  0
                     on  exception event
       
                        if key_up or key_down
                           perform scroll-paper thru scroll-paper-ex
                        end-if
       
                        if key_right and ncif > 0
                           if not fl_vi  move 0 to comodo v(ncif)
                                         subtract 1 from ncif
                                         move importo(1:12)
                                         to comodo(2:12)
                                         move comodo to importo
                              else       move "0" 
                                         to importo(14 + ndec:1)
                                         subtract 1 from ndec
                                         move importo to comodo
                                         if ndec = 0
                                         set fl_vi to false  end-if
                           end-if
       
                           perform vis-num thru vis-num-ex
       
                        end-if
       
                        if e_ret   move "=" to x   end-if
       
              end-accept
       
              evaluate true
       
                 when cnt
                      move counter to comodo
                      move "N"     to x
                      perform vis-num
                      perform carta thru carta-ex
       
                 when no_value
                      perform tempo thru tempo-ex
       
                 when tras_total
                      move 0 to field-size
                      perform varying i from 1 by 1
                              until importo(i:1) not = 0 or i = 14
                         continue
                      end-perform
                      perform varying i from i by 1 until i = 14
                         add 1 to field-size
                         move importo(i:1) to cf(field-size)
                      end-perform
       
                 when help
                      perform help thru help-ex
       
                 when not key_ok
       
                 when ce
                      move 0 to comodo
                      perform vis-num thru vis-num-ex
                      perform ini-var-010 thru ini-var-ex
       
                 when clear
                      close window pua1
                      perform mas thru mas-ex
                      move 0 to z start_list end_list elem_array
                      move z to d2
                      display disp line 2 position 4 high
                      perform ini-var thru ini-var-ex
       
       
                 when virg
                      if not fl_vi  set fl-vi to true end-if
       
                 when cifra
                      if not fl_vi  perform num-int thru num-int-ex
                         else       perform num-dec thru num-dec-ex
                      end-if
       
                 when other
       
                      perform calc thru calc-ex
       
                      move max_dec to ndec
                      move totale to comodo
                      perform vis-num thru vis-num-ex
       
                      if x  = "="     move "T" to x
                                      set reset_counter to true
                                      perform carta thru carta-ex
                                      move "=" to x
                      end-if
       
                      if x not = "=" perform ini-var-010 thru ini-var-ex
                         else        perform ini-var thru ini-var-ex
                      end-if
       
              end-evaluate
       
           end-perform.
       
       cal-ex. exit.
       
       ini-var.
           move totale        to importo.
           move 0             to totale max_dec.
           move 1             to ind.
       
       ini-var-010.
           move  1            to mult.
           move 10            to multd.
           move  0            to ncif ndec.
           set fl_vi          to false.
       
       ini-var-ex.
       
       num-int.
       
           if ncif < 12
              add  1 to ncif
              move c to v(ncif)
              move 1 to mult
              move 0 to importo
       
              perform varying i from ncif by -1 until i = 0
                 multiply v(i) by mult giving comodo
                 add comodo    to importo
                 multiply 10   by mult
              end-perform
       
              move importo to comodo
              perform vis-num thru vis-num-ex
       
           end-if.
       
       num-int-ex.
       
       num-dec.
           add 1 to ndec.
           divide multd into c giving comodo.
           add comodo to importo.
           multiply 10 by multd.
       
           move importo to comodo
           perform vis-num thru vis-num-ex.
       
           if ndec > max_dec   move ndec to max_dec.
       
       num-dec-ex.
       
       calc.
       
           if op-total
              if mult_op
                            perform carta thru carta-ex
                            compute totale = importo * operando
                            move 0 to type_op
              end-if
              if div_op     perform carta thru carta-ex
                            compute totale = operando / importo
                            move 0 to type_op
              end-if
       
              go to calc-ex.
       
           move importo to comodo.
           perform vis-num.
           perform carta thru carta-ex.
       
           if (op_add or op_subtract) and reset_counter
              move 0 to counter
              set reset_counter to false
           end-if
       
           evaluate true
       
              when op_add        add      importo to   totale
                                 add      1       to   counter
              when op_subtract   subtract importo from totale
                                 add      1       to   counter
              when op_multiply   move     importo to   operando
                                          set mult_op  to true
              when op_divide     move     importo to   operando
                                          set div_op   to true
       
           end-evaluate.
       
       calc-ex.   exit.
       
       vis-num.
       
           evaluate true
       
              when decimal0  move comodo to z    move z   to d2
              when decimal1  move comodo to zv1  move zv1 to d2
              when decimal2  move comodo to zv2  move zv2 to d2
              when decimal3  move comodo to zv3  move zv3 to d2
              when decimal4  move comodo to zv4  move zv4 to d2
              when decimal5  move comodo to zv5  move zv5 to d2
       
           end-evaluate.
       
       vn-100.
           display disp at 0204 high.
       
       vis-num-ex.   exit.
       
       carta.
       
           if end_list < elem_array and elem_array > 14
              display window at 0347 size 24 lines 14 no scroll
              compute end_list = elem_array - 13
              perform varying start_list from end_list by 1
                                         until start_list > elem_array
                 display number_c(start_list) position  1 color paper
                 display chr_sign(start_list) position 23 color paper
                                          size 2
              end-perform
           end-if.
       
           perform load-array   thru load-array-ex.
           perform display-elem thru display-elem-ex.
       
           if x = "T"        set line_total to true
                             perform load-array   thru load-array-ex
                             perform display-elem thru display-elem-ex
                             set line_total to false.
       
           display window line 19 column 45 size 28 lines 5.
       
       carta-ex.   exit.
       
       load-array.
       
           add  1          to elem_array.
           move elem_array to end_list.
       
           if elem_array > 14   compute start_list = elem_array - 14.
       
           if elem_array > max_array
       
                     move max_array to elem_array
       
                     move spaces            to array_work
                     move array_scroll(24:) to array_work
                     move array_work        to array_scroll
       
                     move x         to chr_sign(elem_array)
                     move disp      to number_c(elem_array)
       
              else
                     if line_total
                              move space to number_c(elem_array)
                                          chr_sign(elem_array)
                        else  move x     to chr_sign(elem_array)
                              move disp  to number_c(elem_array)
           end-if.
       
       load-array-ex.
       
       display-elem.
       
           if k > 1 and not fl_tv    subtract k from 17 giving k1
              else                   set fl_tv to true
                                     move 3  to k
                                     move 14 to k1.
       
           display window line k position 45 size 29 lines k1 color 40
       
           if line_total
                 display spaces line k1 position 3 size 24 color paper
                                                               scroll up
              else
                 display disp   line k1 position 3 color calc scroll up
                          if op_subtract
                             display omitted line k1 position  3 color
                                                        calc_sub size 22
                             display x       line k1 position 25 color
                                                        calc_sub size 2
                          else
                             display omitted line k1 position  3 color
                                                           paper size 22
                             display x       line k1 position 25 color
                                                           paper size  2
                          end-if
           end-if
       
           display line line k1 position  2 lines 1.
           display line line k1 position 27 lines 1.
       
           subtract 1 from k.
       
       display-elem-ex.
       
       tempo.
       
           accept data_r from date.
           accept ora    from time.
       
           move aa_r to aa_c  move mm_r to mm_c  move gg_r to gg_c.
           move data_c to edidata.
       
           move hh to hhe  move mm to mme  move ss to sse.
       
           display edidata at 0405 color calc
                   " - "   at 0413 color calc
                   ora-d   at 0416 color calc.
       
       tempo-ex.   exit.
       
       help.
       
           display window at 0101 size 80 lines 24.
       
           display window at 0620 size 36 lines 16
                              top centered title
                              "* C A L C O M *"
                           bottom centered title
                            "Calcolatrice Commerciale"
                                        shadow
                                  boxed pop-up pua2.
       
                   display omitted               at 0101
                   "<+> = ADDIZIONE       "      at 0305
                   "<-> = SOTTRAZIONE     "      at 0405
                   "<*> = MOLTIPLICAZIONE "      at 0505
                   "</> = DIVISIONE       "      at 0605
                   "<=> = UGUALE          "      at 0705
                   "<E> = CANCELLA  IMPUTAZIONE" at 0805
                   "<C> = CANCELLA  TUTTO  "     at 0905
                   "<F> = ABBANDONA FUNZIONE"    at 1005
                   "<T> = TRASFERISCI TOTALE"    at 1105
                   "<N> = NUMERO OPERAZIONI"     at 1205
                   " -> = CANCELLA CARATTERE"    at 1405.
       
       
       
      *____ ??????
           display box line 13 position 04 size 34 lines 3.
       
           accept omitted on exception continue
       
           close window pua2.
       
           perform mas200.
       
       help-ex.   exit.
       
       tutto-video.
           display window line 1 position 1 size 80 lines 24.
       
       set-color.
       
           accept terminal_abilities from terminal_info.
           if has_color
              move 168    to calc
              move 257    to paper
              move 261    to calc_sub
           else
              move 1024   to calc
              move 4096   to paper
              move 2048   to calc_sub
           end-if.
       
       set-keyboard.
       
           call "c$keymap" using "1".
       
           set environment "keystroke"   to "Exception=13  ^M".
           set environment "keystroke"   to "Exception=52  ku".
           set environment "keystroke"   to "Exception=53  kd".
           set environment "keystroke"   to "Exception=54  kr".
           set environment "cursor-mode" to "2".
       
       reset-keyboard.
       
           call "c$keymap" using "0".
       
           set environment "cursor-mode" to "1".
       
           close window pua2 no display.
       
           if pua1 not = spaces    close window pua1.
           if pua  not = spaces    close window pua.
       
       
       mas.
           if pua not = spaces close window pua.
       
           display window line 19 column 45 size 28 lines 5
                                        boxed erase color calc
                              bottom right title "<H> help" shadow
                                         pop-up area is pua.
       
           display box line 1 column 3 size 24 lines 3.
       
       mas100.
           display window line 1 position 45 size 29 lines 17 color 40
                                                           pop-up pua1.
       
           display box    line 16 position  2 size  26 lines 2.
           display spaces line 17 position  1 size  28.
           display spaces line 17 position  3 size  25 color paper.
           display line   line 17 position  2 lines  1.
           display line   line 17 position 27 lines  1.
       
           move 15 to k.
           set fl_tv to false.
       
       mas200.
           display window line 19 column 45 size 29 lines 5.
       
       mas-ex.   exit.
       
       scroll-paper.
       
           if k > 1 and not fl_tv go to scroll-paper-ex.
       
           display window at 0347 size 24 lines 14 no wrap no scroll
       
           evaluate true
       
              when key_up
                   if start_list not = 0
                      if chr_subtract(start_list)
                         display number_c(start_list) at 0101 color
                                                               calc_sub
                                                            scroll down
                         display chr_sign(start_list) at 0123 color
                                                        calc_sub size 2
                      else
                         display number_c(start_list) at 0101 color
                                                      paper scroll down
                         display chr_sign(start_list) at 0123 color
                                                      paper size 2
                      end-if
                      subtract 1 from start_list end_list
                   end-if
       
              when key-down
                   if end_list <= elem_array
                      if chr_subtract(end_list)
                         display number_c(end_list) at 1401 color
                                                      calc_sub scroll up
                         display chr_sign(end_list) at 1423 color
                                                      calc_sub size 2
                      else
                         display number_c(end_list) at 1401 color paper
                                                               scroll up
                         display chr_sign(end_list) at 1423 color paper
                                                                  size 2
                      end-if
                      add 1 to start_list end_list
                   end-if
       
           end-evaluate.
       
           display window at 1945 size 28 lines 5.
       
       scroll-paper-ex.
       