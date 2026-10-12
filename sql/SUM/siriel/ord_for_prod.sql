/*   *=============================================*/
/*   * Tabella [ord_for_prod] (711.001)            */
/*   *                                             */
/*   * Aggiornato 23/03/07                         */
/*   *---------------------------------------------*/
  doc_num            varchar(11)   NOT NULL DEFAULT '0' ,
  prog_num           int(5)        NOT NULL DEFAULT '0' ,
  codice_prod        varchar(6)    NOT NULL DEFAULT '' ,
  descr_prod         varchar(40)   NOT NULL DEFAULT '' ,
  quant_ord_for      int(8)        NOT NULL DEFAULT '0' ,
  valuta_for         char(3)       NOT NULL DEFAULT '' ,
  valore_ord_for     varchar(10)   NOT NULL DEFAULT '' ,
  data_cons_ric_for  date          NOT NULL,
  data_cons_prev_for date          NOT NULL,
  flag_conf_for      char(1)       NOT NULL DEFAULT '' ,
  INDEX doc_num (doc_num)