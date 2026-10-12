/*   *=============================================*/
/*   * Tabella [ord_cli_anag_st] (730.001)         */
/*   *                                             */
/*   * Aggiornato 19/02/07                         */
/*   *---------------------------------------------*/
  data_ord           date          NOT NULL,
  doc_num            varchar(11)   NOT NULL DEFAULT '0' ,
  cod_cli            int(7)        NOT NULL DEFAULT '0' ,
  cod_dip            char(2)       NOT NULL DEFAULT '1' ,
  rag_soc_cli        varchar(40)   NOT NULL DEFAULT '' ,
  data_ord_cli       date          NOT NULL,
  rif_ord_cli        varchar(10)   NOT NULL DEFAULT '' ,
  cod_agente         int(4)        NOT NULL DEFAULT '0' ,
  INDEX NewIndex (doc_num)
