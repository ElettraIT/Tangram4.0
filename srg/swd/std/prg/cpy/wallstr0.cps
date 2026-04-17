      *    *************************************************************
      *    * Raccolta di routines di trattamento stringhe              *
      *    * ========================================================= *
      *    *                                                           *
      *    * all-str-asx-000/999 : allineamento stringa a  Sinistra    *
      *    *                                                           *
      *    * all-str-adx-000/999 : allineamento stringa a  Destra      *
      *    *                                                           *
      *    * all-str-cen-000/999 : allineamento stringa al Centro      *
      *    *                                                           *
      *    * ========================================================= *
      *    *                                                           *
      *    * all-str-cat-000/999 : concatenamento di max 10 stringhe   *
      *    *                                                           *
      *    * all-str-csb-000/999 : concatenamento di max 10 stringhe   *
      *    *                       con spazio separatore               *
      *    *                                                           *
      *    * ========================================================= *
      *    *                                                           *
      *    * all-str-ext-000/999 : estrazione di max 10 stringhe       *
      *    *                       con delimitatore                    *
      *    *                                                           *
      *    * all-str-sos-000/999 : sostituzione identificatore         *
      *    *                                                           *
      *    * all-str-lun-000/999 : determinazione lunghezza stringa    *
      *    *                                                           *
      *    * all-str-num-000/999 : controllo se stringa numerica       *
      *    *                                                           *
      *    * all-str-chr-000/999 : controllo se carattere presente     *
      *    *                                                           *
      *    * all-str-cna-000/999 : controllo se stringa alfanumerica   *
      *    *                       con caratteri NON alfanumerici      *
      *    *                                                           *
      *    * ========================================================= *
      *    *                                                           *
      *    * all-str-upp-000/999 : trasformazione stringa in uppercase *
      *    *                                                           *
      *    * all-str-low-000/999 : trasformazione stringa in lowercase *
      *    *                                                           *
      *    * all-str-cpt-000/999 : trasformazione stringa capitalize   *
      *    *                                                           *
      *    * all-str-spz-000/999 : spaziatura stringa (A B C D E)      *
      *    *                                                           *
      *    * ========================================================= *
      *    *                                                           *
      *    * all-str-pad-000/999 : padding di una stringa con 'z'      *
      *    *                                                           *
      *    * all-str-ble-000/999 : determinazione spazi vuoti          *
      *    *                                                           *
      *    * ========================================================= *
      *    *                                                           *
      *    * all-nor-atz-000/999 : normalizzazione campo alfabetico    *
      *    *                                                           *
      *    * all-mch-atz-000/999 : match tra due valori normalizzati   *
      *    *                                                           *
      *    *************************************************************

      *    *===========================================================*
      *    * Routine per l'allineamento di una stringa a sinistra      *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    * Input  : w-all-str-lun = Lunghezza massima della stringa  *
      *    *                                                           *
      *    *          w-all-str-alf = Stringa da allineare             *
      *    *                                                           *
      *    * Output : w-all-str-alf = Stringa allineata                *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       all-str-asx-000.
      *              *-------------------------------------------------*
      *              * Se la stringa in input e' tutta a spaces : u-   *
      *              * scita senza alcuna azione                       *
      *              *-------------------------------------------------*
           if        w-all-str-alf        =    spaces
                     go to all-str-asx-999.
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione della lunghezza della *
      *              * stringa dichiarata                              *
      *              *-------------------------------------------------*
           if        w-all-str-lun        <    001
                     move  001            to   w-all-str-lun          .
           if        w-all-str-lun        >    240
                     move  240            to   w-all-str-lun          .
      *              *-------------------------------------------------*
      *              * Determinazione dell'indice relativo al primo    *
      *              * carattere utile della stringa in input          *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-i01          .
           inspect   w-all-str-alf    tallying w-all-str-i01
                                   for leading spaces                 .
      *              *-------------------------------------------------*
      *              * Determinazione dell'indice relativo all'ultimo  *
      *              * carattere utile della stringa in input          *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-i02          .
           inspect   w-all-str-alf    tallying w-all-str-i02
                                  for trailing spaces                 .
      *              *-------------------------------------------------*
      *              * Determinazione della lunghezza effettiva della  *
      *              * stringa in input, troncandola eventualmente al- *
      *              * la massima lunghezza dichiarata                 *
      *              *-------------------------------------------------*
           move      240                  to   w-all-str-i03          .
           subtract  w-all-str-i01        from w-all-str-i03          .
           subtract  w-all-str-i02        from w-all-str-i03          .
           if        w-all-str-i03        >    w-all-str-lun
                     move  w-all-str-lun  to   w-all-str-i03          .
      *              *-------------------------------------------------*
      *              * Spostamento della stringa utile in input in a-  *
      *              * rea di transito allineata a sinistra            *
      *              *-------------------------------------------------*
           move      spaces               to   w-all-str-wst          .
           add       1                    to   w-all-str-i01          .
           move      w-all-str-alf
                    (w-all-str-i01:
                     w-all-str-i03)       to   w-all-str-wst
                                              (1 : w-all-str-i03)     .
      *              *-------------------------------------------------*
      *              * Normalizzazione a spaces della stringa in out-  *
      *              * put                                             *
      *              *-------------------------------------------------*
           move      spaces               to   w-all-str-alf          .
      *              *-------------------------------------------------*
      *              * Preparazione stringa in outpiut allineata a si- *
      *              * nistra                                          *
      *              *-------------------------------------------------*
           move      w-all-str-wst
                    (1 : w-all-str-i03)   to   w-all-str-alf
                                              (1 : w-all-str-i03)     .
       all-str-asx-999.
           exit.

      *    *===========================================================*
      *    * Routine per l'allineamento di una stringa a destra        *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    * Input  : w-all-str-lun = Lunghezza massima della stringa  *
      *    *                                                           *
      *    *          w-all-str-alf = Stringa da allineare             *
      *    *                                                           *
      *    * Output : w-all-str-alf = Stringa allineata                *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       all-str-adx-000.
      *              *-------------------------------------------------*
      *              * Se la stringa in input e' tutta a spaces : u-   *
      *              * scita senza alcuna azione                       *
      *              *-------------------------------------------------*
           if        w-all-str-alf        =    spaces
                     go to all-str-adx-999.
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione della lunghezza della *
      *              * stringa dichiarata                              *
      *              *-------------------------------------------------*
           if        w-all-str-lun        <    001
                     move  001            to   w-all-str-lun          .
           if        w-all-str-lun        >    240
                     move  240            to   w-all-str-lun          .
      *              *-------------------------------------------------*
      *              * Determinazione dell'indice relativo al primo    *
      *              * carattere utile della stringa in input          *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-i01          .
           inspect   w-all-str-alf    tallying w-all-str-i01
                                   for leading spaces                 .
      *              *-------------------------------------------------*
      *              * Determinazione dell'indice relativo all'ultimo  *
      *              * carattere utile della stringa in input          *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-i02          .
           inspect   w-all-str-alf    tallying w-all-str-i02
                                  for trailing spaces                 .
      *              *-------------------------------------------------*
      *              * Determinazione della lunghezza effettiva della  *
      *              * stringa in input, troncandola eventualmente al- *
      *              * la massima lunghezza dichiarata                 *
      *              *-------------------------------------------------*
           move      240                  to   w-all-str-i03          .
           subtract  w-all-str-i01        from w-all-str-i03          .
           subtract  w-all-str-i02        from w-all-str-i03          .
           if        w-all-str-i03        >    w-all-str-lun
                     move  w-all-str-lun  to   w-all-str-i03          .
      *              *-------------------------------------------------*
      *              * Spostamento della stringa utile in input in a-  *
      *              * rea di transito allineata a sinistra            *
      *              *-------------------------------------------------*
           move      spaces               to   w-all-str-wst          .
           add       1                    to   w-all-str-i01          .
           move      w-all-str-alf
                    (w-all-str-i01:
                     w-all-str-i03)       to   w-all-str-wst
                                              (1 : w-all-str-i03)     .
      *              *-------------------------------------------------*
      *              * Normalizzazione a spaces della stringa in out-  *
      *              * put                                             *
      *              *-------------------------------------------------*
           move      spaces               to   w-all-str-alf          .
      *              *-------------------------------------------------*
      *              * Preparazione stringa allineata a destra         *
      *              *-------------------------------------------------*
           move      w-all-str-lun        to   w-all-str-i02          .
           subtract  w-all-str-i03        from w-all-str-i02          .
           add       1                    to   w-all-str-i02          .
           move      w-all-str-wst
                    (1 : w-all-str-i03)   to   w-all-str-alf
                                              (w-all-str-i02:
                                               w-all-str-i03)         .
       all-str-adx-999.
           exit.

      *    *===========================================================*
      *    * Routine per l'allineamento di una stringa al centro       *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    * Input  : w-all-str-lun = Lunghezza massima della stringa  *
      *    *                                                           *
      *    *          w-all-str-alf = Stringa da allineare             *
      *    *                                                           *
      *    * Output : w-all-str-alf = Stringa allineata al centro      *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       all-str-cen-000.
      *              *-------------------------------------------------*
      *              * Se la stringa in input e' tutta a spaces : u-   *
      *              * scita senza alcuna azione                       *
      *              *-------------------------------------------------*
           if        w-all-str-alf        =    spaces
                     go to all-str-cen-999.
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione della lunghezza della *
      *              * stringa dichiarata                              *
      *              *-------------------------------------------------*
           if        w-all-str-lun        <    001
                     move  001            to   w-all-str-lun          .
           if        w-all-str-lun        >    240
                     move  240            to   w-all-str-lun          .
      *              *-------------------------------------------------*
      *              * Determinazione dell'indice relativo al primo    *
      *              * carattere utile della stringa in input          *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-i01          .
           inspect   w-all-str-alf    tallying w-all-str-i01
                                   for leading spaces                 .
      *              *-------------------------------------------------*
      *              * Determinazione dell'indice relativo all'ultimo  *
      *              * carattere utile della stringa in input          *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-i02          .
           inspect   w-all-str-alf    tallying w-all-str-i02
                                  for trailing spaces                 .
      *              *-------------------------------------------------*
      *              * Determinazione della lunghezza effettiva della  *
      *              * stringa in input, troncandola eventualmente al- *
      *              * la massima lunghezza dichiarata                 *
      *              *-------------------------------------------------*
           move      240                  to   w-all-str-i03          .
           subtract  w-all-str-i01        from w-all-str-i03          .
           subtract  w-all-str-i02        from w-all-str-i03          .
           if        w-all-str-i03        >    w-all-str-lun
                     move  w-all-str-lun  to   w-all-str-i03          .
      *              *-------------------------------------------------*
      *              * Spostamento della stringa utile in input in a-  *
      *              * rea di transito allineata a sinistra            *
      *              *-------------------------------------------------*
           move      spaces               to   w-all-str-wst          .
           add       1                    to   w-all-str-i01          .
           move      w-all-str-alf
                    (w-all-str-i01:
                     w-all-str-i03)       to   w-all-str-wst
                                              (1 : w-all-str-i03)     .
      *              *-------------------------------------------------*
      *              * Normalizzazione a spaces della stringa in out-  *
      *              * put                                             *
      *              *-------------------------------------------------*
           move      spaces               to   w-all-str-alf          .
      *              *-------------------------------------------------*
      *              * Preparazione stringa allineata al centro        *
      *              *-------------------------------------------------*
           move      w-all-str-lun        to   w-all-str-i02          .
           subtract  w-all-str-i03        from w-all-str-i02          .
           divide    2                    into w-all-str-i02          .
           add       1                    to   w-all-str-i02          .
           move      w-all-str-wst
                    (1 : w-all-str-i03)   to   w-all-str-alf
                                              (w-all-str-i02:
                                               w-all-str-i03)         .
       all-str-cen-999.
           exit.

      *    *===========================================================*
      *    * Routine per il concatenamento di max 10 stringhe di max   *
      *    * 80 caratteri ciascuna senza lasciare alcuno spazio di     *
      *    * separazione tra una stringa e l'altra                     *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    *                                                           *
      *    * Input  : w-all-str-lun     = Lunghezza massima della      *
      *    *                              stringa concatenata          *
      *    *                                                           *
      *    *          w-all-str-num     = Numero delle stringhe da     *
      *    *                              concatenare                  *
      *    *                                                           *
      *    *          w-all-str-cat (i) = Valore delle stringhe da     *
      *    *                              concatenare                  *
      *    *                                                           *
      *    * Output : w-all-str-alf     = Valore della stringa con-    *
      *    *                              catenata, allineata a si-    *
      *    *                              nistra                       *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       all-str-cat-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione a spaces della stringa in out-  *
      *              * put                                             *
      *              *-------------------------------------------------*
           move      spaces               to   w-all-str-alf          .
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione del numero di strin-  *
      *              * ghe da concatenare                              *
      *              *-------------------------------------------------*
           if        w-all-str-num        >    10
                     move  10             to   w-all-str-num          .
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione della lunghezza della *
      *              * stringa in output                               *
      *              *-------------------------------------------------*
           if        w-all-str-lun        <    001
                     move  001            to   w-all-str-lun          .
           if        w-all-str-lun        >    240
                     move  240            to   w-all-str-lun          .
      *              *-------------------------------------------------*
      *              * Se zero stringhe da concatenare : uscita a spa- *
      *              * ces                                             *
      *              *-------------------------------------------------*
           if        w-all-str-num        =    zero
                     go to all-str-cat-999.
       all-str-cat-100.
      *              *-------------------------------------------------*
      *              * Se le stringhe in input sono tutte a spaces :   *
      *              * uscita a spaces                                 *
      *              *-------------------------------------------------*
       all-str-cat-110.
           move      zero                 to   w-all-str-inx          .
       all-str-cat-120.
           add       1                    to   w-all-str-inx          .
           if        w-all-str-inx        >    w-all-str-num
                     go to all-str-cat-999.
           if        w-all-str-cat
                    (w-all-str-inx)       =    spaces
                     go to all-str-cat-120.
       all-str-cat-200.
      *              *-------------------------------------------------*
      *              * Inizializzazione indice per esame substringhe   *
      *              * da concatenare                                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-inx          .
      *              *-------------------------------------------------*
      *              * Inizializzazione puntatore su stringa in out-   *
      *              * put                                             *
      *              *-------------------------------------------------*
           move      1                    to   w-all-str-pnt          .
      *              *-------------------------------------------------*
      *              * Inizializzazione del numero residuo di caratte- *
      *              * ri residui concatenabili                        *
      *              *-------------------------------------------------*
           move      w-all-str-lun        to   w-all-str-max          .
       all-str-cat-300.
      *              *-------------------------------------------------*
      *              * Incremento indice su substringa                 *
      *              *-------------------------------------------------*
           add       1                    to   w-all-str-inx          .
      *              *-------------------------------------------------*
      *              * Se oltre il numero di stringhe da concatenare : *
      *              * uscita                                          *
      *              *-------------------------------------------------*
           if        w-all-str-inx        >    w-all-str-num
                     go to all-str-cat-999.
      *              *-------------------------------------------------*
      *              * Se substringa a spaces : la si ignora           *
      *              *-------------------------------------------------*
           if        w-all-str-cat
                    (w-all-str-inx)       =    spaces
                     go to all-str-cat-300.
      *              *-------------------------------------------------*
      *              * Determinazione dell'indice relativo al primo    *
      *              * carattere utile della substringa                *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-i01          .
           inspect   w-all-str-cat
                    (w-all-str-inx)   tallying w-all-str-i01
                                   for leading spaces                 .
      *              *-------------------------------------------------*
      *              * Determinazione dell'indice relativo all'ultimo  *
      *              * carattere utile della stringa in input          *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-i02          .
           inspect   w-all-str-cat
                    (w-all-str-inx)   tallying w-all-str-i02
                                  for trailing spaces                 .
      *              *-------------------------------------------------*
      *              * Determinazione della lunghezza effettiva della  *
      *              * substringa, troncandola eventualmente alla mas- *
      *              * sima lunghezza residua                          *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-i03          .
           subtract  w-all-str-i01        from w-all-str-i03          .
           subtract  w-all-str-i02        from w-all-str-i03          .
           if        w-all-str-i03        >    w-all-str-max
                     move  w-all-str-max  to   w-all-str-i03          .
      *              *-------------------------------------------------*
      *              * Concatenazione                                  *
      *              *-------------------------------------------------*
           add       1                    to   w-all-str-i01          .
           move      w-all-str-cat
                    (w-all-str-inx)
                    (w-all-str-i01:
                     w-all-str-i03)       to   w-all-str-alf
                                              (w-all-str-pnt:
                                               w-all-str-i03)         .
      *              *-------------------------------------------------*
      *              * Incremento puntatore su stringa in output       *
      *              *-------------------------------------------------*
           add       w-all-str-i03        to   w-all-str-pnt          .
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : uscita                    *
      *              *-------------------------------------------------*
           if        w-all-str-pnt        >    w-all-str-lun
                     go to all-str-cat-999.
      *              *-------------------------------------------------*
      *              * Decremento numero residuo di caratteri disponi- *
      *              * bili                                            *
      *              *-------------------------------------------------*
           subtract  w-all-str-i03        from w-all-str-max          .
      *              *-------------------------------------------------*
      *              * Riciclo a substringa successiva                 *
      *              *-------------------------------------------------*
           go to     all-str-cat-300.
       all-str-cat-999.
           exit.

      *    *===========================================================*
      *    * Routine per il concatenamento di max 10 stringhe di max   *
      *    * 80 caratteri ciascuna lasciando uno spazio di separazio-  *
      *    * ne tra una stringa e l'altra                              *
      *    *---------------------------------------------------------- *
      *    *                                                           *
      *    *                                                           *
      *    * Input  : w-all-str-lun     = Lunghezza massima della      *
      *    *                              stringa concatenata          *
      *    *                                                           *
      *    *          w-all-str-num     = Numero delle stringhe da     *
      *    *                              concatenare                  *
      *    *                                                           *
      *    *          w-all-str-cat (i) = Valore delle stringhe da     *
      *    *                              concatenare                  *
      *    *                                                           *
      *    * Output : w-all-str-alf     = Valore della stringa con-    *
      *    *                              catenata, allineata a si-    *
      *    *                              nistra                       *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       all-str-csb-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione a spaces della stringa in out-  *
      *              * put                                             *
      *              *-------------------------------------------------*
           move      spaces               to   w-all-str-alf          .
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione del numero di strin-  *
      *              * ghe da concatenare                              *
      *              *-------------------------------------------------*
           if        w-all-str-num        >    10
                     move  10             to   w-all-str-num          .
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione della lunghezza della *
      *              * stringa in output                               *
      *              *-------------------------------------------------*
           if        w-all-str-lun        <    001
                     move  001            to   w-all-str-lun          .
           if        w-all-str-lun        >    240
                     move  240            to   w-all-str-lun          .
      *              *-------------------------------------------------*
      *              * Se zero stringhe da concatenare : uscita a spa- *
      *              * ces                                             *
      *              *-------------------------------------------------*
           if        w-all-str-num        =    zero
                     go to all-str-csb-999.
       all-str-csb-100.
      *              *-------------------------------------------------*
      *              * Se le stringhe in input sono tutte a spaces :   *
      *              * uscita a spaces                                 *
      *              *-------------------------------------------------*
       all-str-csb-110.
           move      zero                 to   w-all-str-inx          .
       all-str-csb-120.
           add       1                    to   w-all-str-inx          .
           if        w-all-str-inx        >    w-all-str-num
                     go to all-str-csb-999.
           if        w-all-str-cat
                    (w-all-str-inx)       =    spaces
                     go to all-str-csb-120.
       all-str-csb-200.
      *              *-------------------------------------------------*
      *              * Inizializzazione indice per esame substringhe   *
      *              * da concatenare                                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-inx          .
      *              *-------------------------------------------------*
      *              * Inizializzazione puntatore su stringa in out-   *
      *              * put                                             *
      *              *-------------------------------------------------*
           move      1                    to   w-all-str-pnt          .
      *              *-------------------------------------------------*
      *              * Inizializzazione del numero residuo di caratte- *
      *              * ri residui concatenabili                        *
      *              *-------------------------------------------------*
           move      w-all-str-lun        to   w-all-str-max          .
       all-str-csb-300.
      *              *-------------------------------------------------*
      *              * Incremento indice su substringa                 *
      *              *-------------------------------------------------*
           add       1                    to   w-all-str-inx          .
      *              *-------------------------------------------------*
      *              * Se oltre il numero di stringhe da concatenare : *
      *              * uscita                                          *
      *              *-------------------------------------------------*
           if        w-all-str-inx        >    w-all-str-num
                     go to all-str-csb-999.
      *              *-------------------------------------------------*
      *              * Se substringa a spaces : la si ignora           *
      *              *-------------------------------------------------*
           if        w-all-str-cat
                    (w-all-str-inx)       =    spaces
                     go to all-str-csb-300.
      *              *-------------------------------------------------*
      *              * Determinazione dell'indice relativo al primo    *
      *              * carattere utile della substringa                *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-i01          .
           inspect   w-all-str-cat
                    (w-all-str-inx)   tallying w-all-str-i01
                                   for leading spaces                 .
      *              *-------------------------------------------------*
      *              * Determinazione dell'indice relativo all'ultimo  *
      *              * carattere utile della stringa in input          *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-i02          .
           inspect   w-all-str-cat
                    (w-all-str-inx)   tallying w-all-str-i02
                                  for trailing spaces                 .
      *              *-------------------------------------------------*
      *              * Determinazione della lunghezza effettiva della  *
      *              * substringa, troncandola eventualmente alla mas- *
      *              * sima lunghezza residua                          *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-i03          .
           subtract  w-all-str-i01        from w-all-str-i03          .
           subtract  w-all-str-i02        from w-all-str-i03          .
           if        w-all-str-i03        >    w-all-str-max
                     move  w-all-str-max  to   w-all-str-i03          .
      *              *-------------------------------------------------*
      *              * Concatenazione                                  *
      *              *-------------------------------------------------*
           add       1                    to   w-all-str-i01          .
           move      w-all-str-cat
                    (w-all-str-inx)
                    (w-all-str-i01:
                     w-all-str-i03)       to   w-all-str-alf
                                              (w-all-str-pnt:
                                               w-all-str-i03)         .
      *              *-------------------------------------------------*
      *              * Incremento puntatore su stringa in output       *
      *              *-------------------------------------------------*
           add       w-all-str-i03        to   w-all-str-pnt          .
           add       1                    to   w-all-str-pnt          .
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : uscita                    *
      *              *-------------------------------------------------*
           if        w-all-str-pnt        >    w-all-str-lun
                     go to all-str-csb-999.
      *              *-------------------------------------------------*
      *              * Decremento numero residuo di caratteri disponi- *
      *              * bili                                            *
      *              *-------------------------------------------------*
           subtract  w-all-str-i03        from w-all-str-max          .
           subtract  1                    from w-all-str-max          .
      *              *-------------------------------------------------*
      *              * Riciclo a substringa successiva                 *
      *              *-------------------------------------------------*
           go to     all-str-csb-300.
       all-str-csb-999.
           exit.

      *    *===========================================================*
      *    * Routine per l'estrazione di max 10 stringhe separate da   *
      *    * un delimitatore                                           *
      *    *---------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : w-all-str-alf     = Valore della stringa da cui  *
      *    *                              estrarre                     *
      *    *                                                           *
      *    *          w-all-str-del     = Delimitatore                 *
      *    *                                                           *
      *    * Output : w-all-str-cat (i) = Stringhe estratte            *
      *    *                                                           *
      *    *          w-all-str-num     = Numero stringhe estratte     *
      *    *-----------------------------------------------------------*
       all-str-ext-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-num          .
           move      spaces               to   w-all-str-cat (01)     .
           move      spaces               to   w-all-str-cat (02)     .
           move      spaces               to   w-all-str-cat (03)     .
           move      spaces               to   w-all-str-cat (04)     .
           move      spaces               to   w-all-str-cat (05)     .
           move      spaces               to   w-all-str-cat (06)     .
           move      spaces               to   w-all-str-cat (07)     .
           move      spaces               to   w-all-str-cat (08)     .
           move      spaces               to   w-all-str-cat (09)     .
           move      spaces               to   w-all-str-cat (10)     .
      *              *-------------------------------------------------*
      *              * Test preliminare se il campo e' vuoto           *
      *              *-------------------------------------------------*
           if        w-all-str-alf        =    spaces
                     go to all-str-ext-900.
       all-str-ext-100.
      *              *-------------------------------------------------*
      *              * Estrazione                                      *
      *              *-------------------------------------------------*
           unstring  w-all-str-alf
                                delimited by   w-all-str-del
                                          into w-all-str-cat (01)
                                               w-all-str-cat (02)
                                               w-all-str-cat (03)
                                               w-all-str-cat (04)
                                               w-all-str-cat (05)
                                               w-all-str-cat (06)
                                               w-all-str-cat (07)
                                               w-all-str-cat (08)
                                               w-all-str-cat (09)
                                               w-all-str-cat (10)     .
      *              *-------------------------------------------------*
      *              * Ciclo di verifica numero stringhe estratte      *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-i01          .
       all-str-ext-200.
           add       1                    to   w-all-str-i01          .
           if        w-all-str-i01        >    10
                     go to all-str-ext-900.
           if        w-all-str-cat
                    (w-all-str-i01)       =    spaces
                     go to all-str-ext-900.
           add       1                    to   w-all-str-num          .
       all-str-ext-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     all-str-ext-200.
       all-str-ext-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     all-str-ext-999.
       all-str-ext-999.
           exit.

      *    *===========================================================*
      *    * Routine per la sostituzione di un identificatore con una  *
      *    * stringa                                                   *
      *    *---------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : w-all-str-lun     = Lunghezza massima della      *
      *    *                              stringa da trattare          *
      *    *                                                           *
      *    *          w-all-str-alf     = Stringa da trattare          *
      *    *                                                           *
      *    *          w-all-str-cat (1) = Identificatore               *
      *    *                                                           *
      *    *          w-all-str-cat (2) = Sostitutivo                  *
      *    *                                                           *
      *    *                                                           *
      *    * Output : w-all-str-alf     = Stringa risultante           *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       all-str-sos-000.
      *              *-------------------------------------------------*
      *              * Se la stringa in input e' tutta a spaces : u-   *
      *              * scita senza alcuna azione                       *
      *              *-------------------------------------------------*
           if        w-all-str-alf        =    spaces
                     go to all-str-sos-999.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-i01          .
           move      w-all-str-cat (1)    to   w-all-str-del          .
      *              *-------------------------------------------------*
      *              * Verifica preliminare                            *
      *              *-------------------------------------------------*
           inspect   w-all-str-alf    tallying w-all-str-i01
                                     for all   w-all-str-del          .
           if        w-all-str-i01        not  = 001
                     go to all-str-sos-999.
      *              *-------------------------------------------------*
      *              * Preparazione estrazione                         *
      *              *-------------------------------------------------*
           move      spaces               to   w-all-str-wst          .
           move      spaces               to   w-all-str-ws2          .
           move      zero                 to   w-all-str-i02          .
      *              *-------------------------------------------------*
      *              * Estrazione                                      *
      *              *-------------------------------------------------*
           unstring  w-all-str-alf
                                delimited by   w-all-str-del
                                          into w-all-str-wst
                                    count in   w-all-str-i02          .
           add       2                    to   w-all-str-i02          .
           unstring  w-all-str-alf        into w-all-str-ws2
                                  with pointer w-all-str-i02          .
      *              *-------------------------------------------------*
      *              * Concatenamento                                  *
      *              *-------------------------------------------------*
           move      03                   to   w-all-str-num          .
           move      w-all-str-wst        to   w-all-str-cat (1)      .
           move      w-all-str-ws2        to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
       all-str-sos-999.
           exit.

      *    *===========================================================*
      *    * Routine per la determinazione della lunghezza di una      *
      *    * stringa alfanumerica                                      *
      *    *---------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : w-all-str-alf     = Stringa da misurare          *
      *    *                                                           *
      *    *                                                           *
      *    * Output : w-all-str-lun     = Lunghezza risultante         *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       all-str-lun-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-lun          .
      *              *-------------------------------------------------*
      *              * Se la stringa in input e' tutta a spaces : u-   *
      *              * scita senza alcuna azione                       *
      *              *-------------------------------------------------*
           if        w-all-str-alf        =    spaces
                     go to all-str-lun-999.
      *              *-------------------------------------------------*
      *              * Determinazione                                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-i01          .
           inspect   w-all-str-alf    tallying w-all-str-i01
                                  for trailing spaces                 .
           move      240                  to   w-all-str-lun          .
           subtract  w-all-str-i01        from w-all-str-lun          .
       all-str-lun-999.
           exit.

      *    *===========================================================*
      *    * Routine per la determinazione se una stringa alfanumerica *
      *    * e' interamente numerica                                   *
      *    *---------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : w-all-str-alf     = Stringa da trattare          *
      *    *                                                           *
      *    *                                                           *
      *    * Output : w-all-str-flg     = Flag risultante              *
      *    *                              '#' = Non numerica           *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       all-str-num-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-all-str-flg          .
           move      18                   to   w-all-str-i01          .
       all-str-num-100.
           if        w-all-str-alf
                    (w-all-str-i01 : 01)  not  = spaces
                     go to all-str-num-200.
           subtract  1                    from w-all-str-i01          .
           if        w-all-str-i01        =    zero
                     go to all-str-num-999
           else      go to all-str-num-100.
       all-str-num-200.
           if        w-all-str-alf
                    (w-all-str-i01 : 01)  <    "0" or
                     w-all-str-alf
                    (w-all-str-i01 : 01)  >    "9"
                     move  "#"            to   w-all-str-flg
                     go to all-str-num-999.
           subtract  1                    from w-all-str-i01          .
           if        w-all-str-i01        =    zero
                     go to all-str-num-999
           else      go to all-str-num-200.
       all-str-num-999.
           exit.

      *    *===========================================================*
      *    * Routine per la determinazione se una stringa contiene un  *
      *    * determinato carattere                                     *
      *    *---------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : w-all-str-alf     = Stringa da trattare          *
      *    *                                                           *
      *    *          w-all-str-del     = Carattere da individuare     *
      *    *                                                           *
      *    *                                                           *
      *    * Output : w-all-str-num     = Numero di occorrenze         *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       all-str-chr-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-num          .
      *              *-------------------------------------------------*
      *              * Test                                            *
      *              *-------------------------------------------------*
           inspect   w-all-str-alf    tallying w-all-str-num
                                     for all   w-all-str-del          .
       all-str-chr-999.
           exit.

      *    *===========================================================*
      *    * Routine per la determinazione se una stringa contiene una *
      *    * serie di caratteri non alfabetici                         *
      *    *---------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : w-all-str-alf     = Stringa da trattare          *
      *    *                                                           *
      *    *          w-all-str-lun     = Lunghezza massima della      *
      *    *                              stringa da trattare          *
      *    *                                                           *
      *    *                                                           *
      *    * Output : w-all-str-flg     = Flag risultante              *
      *    *                              '#' = Non alfabetica         *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       all-str-cna-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-all-str-flg          .
           move      w-all-str-lun        to   w-all-str-i01          .
       all-str-cna-100.
           if        w-all-str-alf
                    (w-all-str-i01 : 01)  not  = spaces
                     go to all-str-cna-200.
           subtract  1                    from w-all-str-i01          .
           if        w-all-str-i01        =    zero
                     go to all-str-cna-999
           else      go to all-str-cna-100.
       all-str-cna-200.
           if        w-all-str-alf
                    (w-all-str-i01 : 01)  <    "0" or
                     w-all-str-alf
                    (w-all-str-i01 : 01)  >    "Z"
                     move  "#"            to   w-all-str-flg
                     go to all-str-cna-999.
           subtract  1                    from w-all-str-i01          .
           if        w-all-str-i01        =    zero
                     go to all-str-cna-999
           else      go to all-str-cna-200.
       all-str-cna-999.
           exit.

      *    *===========================================================*
      *    * Routine per la trasformazione in Uppercase di una stringa * 
      *    *---------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : w-all-str-lun     = Lunghezza massima della      *
      *    *                              stringa da trattare          *
      *    *                                                           *
      *    *          w-all-str-alf     = Stringa da trattare          *
      *    *                                                           *
      *    *                                                           *
      *    * Output : w-all-str-alf     = Stringa in uppercase         *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       all-str-upp-000.
      *              *-------------------------------------------------*
      *              * Scansione stringa                               *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-i01          .
       all-str-upp-100.
           add       1                    to   w-all-str-i01          .
           if        w-all-str-i01        >    w-all-str-lun
                     go to  all-str-upp-999.
           move      zero                 to   w-all-str-i02          .
           inspect   w-all-str-low    tallying w-all-str-i02
                     for characters     before initial w-all-str-alf
                                                      (w-all-str-i01 :
                                                       01)            .
      *              *-------------------------------------------------*
      *              * Trasformazione carattere stringa                *
      *              *-------------------------------------------------*
           if        w-all-str-i02        <    26
                     add     1            to   w-all-str-i02
                     move    w-all-str-upc
                            (w-all-str-i02)
                                          to   w-all-str-alf
                                              (w-all-str-i01 : 01)    .
           go to     all-str-upp-100.
       all-str-upp-999.
           exit.

      *    *===========================================================*
      *    * Routine per la trasformazione in Lowercase di una stringa * 
      *    *---------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : w-all-str-lun     = Lunghezza massima della      *
      *    *                              stringa da trattare          *
      *    *                                                           *
      *    *          w-all-str-alf     = Stringa da trattare          *
      *    *                                                           *
      *    *                                                           *
      *    * Output : w-all-str-alf     = Stringa in lowercase         *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       all-str-low-000.
      *              *-------------------------------------------------*
      *              * Scansione stringa                               *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-i01          .
       all-str-low-100.
           add       1                    to   w-all-str-i01          .
           if        w-all-str-i01        >    w-all-str-lun
                     go to  all-str-low-999.
           move      zero                 to   w-all-str-i02          .
           inspect   w-all-str-upp    tallying w-all-str-i02
                     for characters     before initial w-all-str-alf
                                                      (w-all-str-i01 :
                                                       01)            .
      *              *-------------------------------------------------*
      *              * Trasformazione carattere stringa                *
      *              *-------------------------------------------------*
           if        w-all-str-i02        <    26
                     add   1              to   w-all-str-i02
                     move  w-all-str-loc
                          (w-all-str-i02) to   w-all-str-alf
                                              (w-all-str-i01 : 01)    .
           go to     all-str-low-100.
       all-str-low-999.
           exit.

      *    *===========================================================*
      *    * Routine per la trasformazione in Capitalize della stringa * 
      *    *---------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : w-all-str-lun     = Lunghezza massima della      *
      *    *                              stringa da trattare          *
      *    *                                                           *
      *    *          w-all-str-alf     = Stringa da trattare          *
      *    *                                                           *
      *    *                                                           *
      *    * Output : w-all-str-alf     = Stringa capitalized          *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       all-str-cpt-000.
      *              *-------------------------------------------------*
      *              * Scansione stringa                               *
      *              *-------------------------------------------------*
           move      1                    to   w-all-str-i01          .
       all-str-cpt-100.
           add       1                    to   w-all-str-i01          .
           if        w-all-str-i01        >    w-all-str-lun
                     go to  all-str-cpt-999.
      *
           if        w-all-str-alf
                    (w-all-str-i01 : 01)  =    spaces or
                     w-all-str-alf
                    (w-all-str-i01 : 01)  =    "'"
                     add  1               to   w-all-str-i01
                     go to all-str-cpt-100.
      *
           move      zero                 to   w-all-str-i02          .
           inspect   w-all-str-upp    tallying w-all-str-i02
                     for characters     before initial w-all-str-alf
                                                      (w-all-str-i01 :
                                                       01)            .
           if        w-all-str-i02        <    26
                     add     1            to   w-all-str-i02
                     move    w-all-str-loc
                            (w-all-str-i02)
                                          to   w-all-str-alf
                                              (w-all-str-i01 : 01)    .
           go to     all-str-cpt-100.
       all-str-cpt-999.
           exit.

      *    *===========================================================*
      *    * Routine per la spaziatura di una stringa con un separato- * 
      *    * re blank tra un carattere e l'altro (A B C D E)           * 
      *    *---------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : w-all-str-lun     = Lunghezza massima della      *
      *    *                              stringa da trattare          *
      *    *                                                           *
      *    *          w-all-str-alf     = Stringa da trattare          *
      *    *                                                           *
      *    *          w-all-str-num     = Caratteri di spaziatura      *
      *    *                                                           *
      *    *                                                           *
      *    * Output : w-all-str-alf     = Stringa spaziata             *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       all-str-spz-000.
      *              *-------------------------------------------------*
      *              * Salvataggio preliminare stringa                 *
      *              *-------------------------------------------------*
           move      w-all-str-alf        to   w-all-str-wst          .
           move      spaces               to   w-all-str-alf          .
           add       1                    to   w-all-str-num          .
      *              *-------------------------------------------------*
      *              * Scansione stringa                               *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-i01          .
           move      1                    to   w-all-str-i02          .
       all-str-spz-100.
           add       1                    to   w-all-str-i01          .
           add       w-all-str-num        to   w-all-str-i02          .
           if        w-all-str-i01        >    w-all-str-lun
                     go to  all-str-spz-999.
      *              *-------------------------------------------------*
      *              * Spaziatura stringa                              *
      *              *-------------------------------------------------*
           move      w-all-str-wst
                    (w-all-str-i01 : 01)  to   w-all-str-alf
                                              (w-all-str-i02 : 01)
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     all-str-spz-100.
       all-str-spz-999.
           exit.

      *    *===========================================================*
      *    * Routine per la determinazione se ci sono spazi vuoti al-  *
      *    * l'interno di una stringa alfanumerica                     *
      *    *---------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : w-all-str-lun     = Lunghezza massima della      *
      *    *                              stringa da trattare          *
      *    *                                                           *
      *    *          w-all-str-alf     = Stringa da trattare          *
      *    *                                                           *
      *    *                                                           *
      *    * Output : w-all-str-flg     = Flag risultante              *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       all-str-ble-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-all-str-flg          .
      *              *-------------------------------------------------*
      *              * Test se stringa vuota                           *
      *              *-------------------------------------------------*
           if        w-all-str-alf        =    spaces
                     go to all-str-ble-999.
      *              *-------------------------------------------------*
      *              * Test su primo carattere stringa                 *
      *              *-------------------------------------------------*
           if        w-all-str-alf
                    (01 : 01)             =    spaces
                     move  "#"            to   w-all-str-flg
                     go to all-str-ble-999.
      *              *-------------------------------------------------*
      *              * Scansione stringa                               *
      *              *-------------------------------------------------*
           move      1                    to   w-all-str-i01          .
       all-str-ble-100.
           add       1                    to   w-all-str-i01          .
           if        w-all-str-i01        >    w-all-str-lun
                     go to all-str-ble-999.
           if        w-all-str-alf
                    (w-all-str-i01 : 01)  not  = spaces
                     go to all-str-ble-100.
       all-str-ble-200.
           add       1                    to   w-all-str-i01          .
           if        w-all-str-i01        >    w-all-str-lun
                     go to all-str-ble-999.
           if        w-all-str-alf
                    (w-all-str-i01 : 01)  =    spaces
                     go to all-str-ble-200.
      *              *-------------------------------------------------*
      *              * Flag di spazio individuato                      *
      *              *-------------------------------------------------*
           move      "#"                  to   w-all-str-flg          .
       all-str-ble-999.
           exit.

      *    *===========================================================*
      *    * Routine per il padding di una stringa (riempimento di 'z')* 
      *    *---------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : w-all-str-lun     = Lunghezza massima della      *
      *    *                              stringa da trattare          *
      *    *                                                           *
      *    *          w-all-str-alf     = Stringa da trattare          *
      *    *                                                           *
      *    *                                                           *
      *    * Output : w-all-str-alf     = Stringa trattata             *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       all-str-pad-000.
      *              *-------------------------------------------------*
      *              * Scansione stringa                               *
      *              *-------------------------------------------------*
           move      w-all-str-lun        to   w-all-str-i01          .
       all-str-pad-200.
           if        w-all-str-i01        >    zero
                     if    w-all-str-alf
                          (w-all-str-i01 : 01)
                                          =    spaces
                           move    "z"    to   w-all-str-alf
                                              (w-all-str-i01 : 01)
                           subtract 1     from w-all-str-i01
                           go to all-str-pad-200.
       all-str-pad-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione di un campo alfabetico in un valore privo *
      *    * di spaces e di caratteri non compresi tra i limiti A..Z   *
      *    * oppure 0..9                                               *
      *    *---------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : w-all-str-lun     = Lunghezza massima della      *
      *    *                              stringa da trattare          *
      *    *                                                           *
      *    *          w-all-str-alf     = Stringa da normalizzare      *
      *    *                                                           *
      *    *                                                           *
      *    * Output : w-all-str-alf     = Stringa normalizzata         *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       all-nor-atz-000.
      *              *-------------------------------------------------*
      *              * Trasformazione in uppercase del valore di ori-  *
      *              * gine                                            *
      *              *-------------------------------------------------*
           perform   all-str-upp-000      thru all-str-upp-999        .
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare valore di destina-  *
      *              * zione                                           *
      *              *-------------------------------------------------*
           move      spaces               to   w-all-str-wst          .
      *              *-------------------------------------------------*
      *              * Indice su valore di origine a zero              *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-i01          .
      *              *-------------------------------------------------*
      *              * Indice su valore di destinazione a zero         *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-i02          .
       all-nor-atz-100.
      *              *-------------------------------------------------*
      *              * Incremento indice su valore di origine          *
      *              *-------------------------------------------------*
           add       1                    to   w-all-str-i01          .
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : uscita                    *
      *              *-------------------------------------------------*
           if        w-all-str-i01        >    w-all-str-lun
                     go to all-nor-atz-900.
      *              *-------------------------------------------------*
      *              * Se il carattere di origine in esame e' compreso *
      *              * tra A..Z, lo si sposta nella destinazione e si  *
      *              * ricicla sul carattere di origine successivo     *
      *              *-------------------------------------------------*
           if        w-all-str-alf
                    (w-all-str-i01 : 01)  not  < "A" and
                     w-all-str-alf
                    (w-all-str-i01 : 01)  not  > "Z"
                     add   1              to   w-all-str-i02
                     move  w-all-str-alf
                          (w-all-str-i01 : 01)
                                          to   w-all-str-wst
                                              (w-all-str-i02 : 01)
                     go to all-nor-atz-100.
      *              *-------------------------------------------------*
      *              * Se il carattere di origine in esame e' compreso *
      *              * tra 0..9, lo si sposta nella destinazione e si  *
      *              * ricicla sul carattere di origine successivo     *
      *              *-------------------------------------------------*
           if        w-all-str-alf
                    (w-all-str-i01 : 01)  not  < "0" and
                     w-all-str-alf
                    (w-all-str-i01 : 01)  not  > "9"
                     add   1              to   w-all-str-i02
                     move  w-all-str-alf
                          (w-all-str-i01 : 01)
                                          to   w-all-str-wst
                                              (w-all-str-i02 : 01)
                     go to all-nor-atz-100.
      *              *-------------------------------------------------*
      *              * In ogni altro caso si ignora il carattere di o- *
      *              * rigine e si ricicla sul carattere di origine    *
      *              * successivo                                      *
      *              *-------------------------------------------------*
           go to     all-nor-atz-100.
       all-nor-atz-900.
      *              *-------------------------------------------------*
      *              * In valore di uscita                             *
      *              *-------------------------------------------------*
           move      w-all-str-wst        to   w-all-str-alf          .
       all-nor-atz-999.
           exit.

      *    *===========================================================*
      *    * Match tra due valori normalizzati A..Z - 0..9 di max 80   *
      *    *---------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : w-all-str-cat (1) = Prima stringa per confronto  *
      *    *                                                           *
      *    *          w-all-str-cat (2) = Seconda stringa per          *
      *    *                              confronto                    *
      *    *                                                           *
      *    *                                                           *
      *    * Output : w-all-str-flg     = risultato confronto          *
      *    *                              - 'N'    : esito negativo    *
      *    *                              - spaces : esito positivo    *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       all-mch-atz-000.
           move      zero                 to   w-all-str-i01          .
           inspect   w-all-str-cat (1)    tallying w-all-str-i01
                                  for trailing spaces                 .
           move      80                   to   w-all-str-i02          .
           subtract  w-all-str-i01        from w-all-str-i02          .
           move      zero                 to   w-all-str-i03          .
           inspect   w-all-str-cat (2)  
                                      tallying w-all-str-i03
                                       for all w-all-str-cat (1)
                                              (01 : w-all-str-i02)    .
           if        w-all-str-i03        =    zero
                     move  "N"            to   w-all-str-flg
           else      move  spaces         to   w-all-str-flg          .
       all-mch-atz-999.
           exit.
