/*   *=============================================*/
/*   * Tabella [ord_cli_prod] (721.001)            */
/*   *                                             */
/*   * Aggiornato 23/03/07                         */
/*   *---------------------------------------------*/
  doc_num            varchar(11)   NOT NULL DEFAULT '0' ,
  prog_num           int(5)        NOT NULL DEFAULT '0' ,
  codice_prod        varchar(6)    NOT NULL DEFAULT '' ,
  cod_prod_cli       varchar(14)   NOT NULL DEFAULT '' ,
  descr_prod         varchar(40)   NOT NULL DEFAULT '' ,
  quant_ord_cli      int(8)        NOT NULL DEFAULT '0' ,
  valuta             char(3)       NOT NULL DEFAULT '' ,
  valore_ord         varchar(10)   NOT NULL DEFAULT '' ,
  data_cons_ric_cli  date          NOT NULL,
  data_cons_prev_cli date          NOT NULL,
  flag_conf_cli      char(1)       NOT NULL DEFAULT '' ,
  INDEX doc_num (doc_num)

