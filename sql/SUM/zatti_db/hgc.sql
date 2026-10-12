/*   *=============================================*/
/*   * Tabella [disp_prodotti] (700.001)           */
/*   *                                             */
/*   * Aggiornato 23/03/07                         */
/*   *---------------------------------------------*/
 ide_dat             int(07)       NOT NULL DEFAULT '0' ,
 ide_ute             varchar(08)   NOT NULL,
 ide_fas             varchar(06)   NOT NULL,
 cod_age             int(07)       NOT NULL DEFAULT '0' ,
 cod_grc             int(03)       NOT NULL DEFAULT '0' ,
 num_grc             int(03)       NOT NULL DEFAULT '0' ,
 num_prg             int(05)       NOT NULL DEFAULT '0' ,
 cod_cli             int(07)       NOT NULL DEFAULT '0' ,
 dpz_cli             varchar(04)   NOT NULL
