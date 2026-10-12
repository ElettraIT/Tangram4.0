/*   *=============================================*/
/*   * Tabella [ord_for_anag] (710.001)            */
/*   *                                             */
/*   * Aggiornato 23/03/07                         */
/*   *---------------------------------------------*/
  data_ord           date          NOT NULL,
  doc_num            varchar(11)   NOT NULL DEFAULT '0' ,
  cod_for            int(7)        NOT NULL DEFAULT '0' ,
  cod_dip            char(2)       NOT NULL DEFAULT '1' ,
  rag_soc_for        varchar(40)   NOT NULL DEFAULT '' ,
  data_ord_for       date          NOT NULL,
  rif_conf_for       varchar(10)   NOT NULL DEFAULT '' ,
  INDEX doc_num (doc_num)
