/*   *=============================================*/
/*   * Tabella [pro_disp_prod]                     */
/*   *                                             */
/*   * Aggiornato 23/03/07                         */
/*   *---------------------------------------------*/
  id                 mediumint(6)         UNSIGNED
                                          AUTO_INCREMENT
                                          PRIMARY KEY,
  order_cons_prev    date                 NOT NULL,
  doc_num            varchar(11)          NOT NULL,
  data_ord           date                 NOT NULL,
  codice_prod        varchar(06)          NOT NULL,
  cons_ric           date                 NOT NULL,
  cons_prev          date                 NOT NULL,
  flag_conf          char(01)             NOT NULL,
  cod_cli            varchar(07)          NOT NULL,
  cod_for            varchar(07)          NOT NULL,
  ana_cli_for        varchar(15)          NOT NULL,
  rif_ord_cli        varchar(10)          NOT NULL,
  data_ord_cli       date                 NOT NULL,
  quant_imp          int(14)              NOT NULL,
  valuta             char(3)              NOT NULL,
  valore_ord         varchar(10)          NOT NULL,
  quant_ord          int(14)              NOT NULL,
  cod_agente         int(04)              NOT NULL,
  INDEX doc_num (doc_num)
