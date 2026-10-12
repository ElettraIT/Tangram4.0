/*    *==========================================================*/
/*    * Tabella [oiv] Voucher autorizzazioni AIA            {aia} */
/*    *                                                          */
/*    * Registra le autorizzazioni concesse per il superamento   */
/*    * di un segnale AIA riferito ad uno specifico oggetto      */
/*    * gestionale.                                              */
/*    *----------------------------------------------------------*/
CREATE TABLE IF NOT EXISTS `oiv` (

/*       *=======================================================*/
/*       * Identificativo voucher                                */
/*       *                                                       */
/*       * Progressivo univoco dell'autorizzazione AIA.          */
/*       *-------------------------------------------------------*/
  `id_oiv`                  bigint unsigned NOT NULL AUTO_INCREMENT
                                            COMMENT 'Identificativo voucher',

/*       *-------------------------------------------------------*/
/*       * Codice segnale AIA                                    */
/*       *                                                       */
/*       * Identifica il segnale per il quale è stata concessa   */
/*       * l'autorizzazione.                                     */
/*       * Esempio: CLI_ORD_BLK                                  */
/*       *-------------------------------------------------------*/
  `cod_sig`                 varchar (20)    NOT NULL
                                            COMMENT 'Codice segnale AIA',

/*       *-------------------------------------------------------*/
/*       * Tipo oggetto gestionale                               */
/*       *                                                       */
/*       * Identifica la natura dell'oggetto cui si riferisce    */
/*       * il voucher.                                           */
/*       * Esempio: ORC                                         */
/*       *-------------------------------------------------------*/
  `obj_typ`                 varchar (10)    NOT NULL
                                            COMMENT 'Tipo oggetto gestionale',

/*       *-------------------------------------------------------*/
/*       * Chiave oggetto gestionale                             */
/*       *                                                       */
/*       * Identifica l'oggetto specifico cui si riferisce       */
/*       * il voucher.                                           */
/*       * Esempio: numero protocollo ordine cliente.            */
/*       *-------------------------------------------------------*/
  `obj_key`                 varchar (30)    NOT NULL
                                            COMMENT 'Chiave oggetto gestionale',

/*       *-------------------------------------------------------*/
/*       * Dipendenza oggetto                                    */
/*       *                                                       */
/*       * Eventuale dipendenza aziendale dell'oggetto           */
/*       * autorizzato.                                          */
/*       *-------------------------------------------------------*/
  `obj_dpz`                 varchar (10)    NOT NULL DEFAULT ''
                                            COMMENT 'Dipendenza oggetto',

/*       *-------------------------------------------------------*/
/*       * Utente autorizzante                                   */
/*       *                                                       */
/*       * Utente che ha concesso il superamento del segnale.    */
/*       *-------------------------------------------------------*/
  `cod_ute`                 varchar (20)    NOT NULL
                                            COMMENT 'Utente autorizzante',

/*       *-------------------------------------------------------*/
/*       * Data e ora autorizzazione                             */
/*       *                                                       */
/*       * Momento nel quale è stato concesso il voucher.        */
/*       *-------------------------------------------------------*/
  `dat_aut`                 datetime        NOT NULL
                                            COMMENT 'Data e ora autorizzazione',

/*       *-------------------------------------------------------*/
/*       * Motivazione autorizzazione                            */
/*       *                                                       */
/*       * Eventuale motivazione indicata dall'autorizzatore.    */
/*       *-------------------------------------------------------*/
  `mot_aut`                 varchar (250)   DEFAULT NULL
                                            COMMENT 'Motivazione autorizzazione',

/*       *-------------------------------------------------------*/
/*       * Stato voucher                                         */
/*       *                                                       */
/*       * A = Autorizzato                                       */
/*       * R = Revocato                                          */
/*       *-------------------------------------------------------*/
  `sta_oiv`                 char (01)       NOT NULL DEFAULT 'A'
                                            COMMENT 'Stato voucher',

/*       *=======================================================*/
/*       * Definizione chiavi                                    */
/*       *-------------------------------------------------------*/
  PRIMARY KEY (`id_oiv`),

/*       *-------------------------------------------------------*/
/*       * Ricerca voucher per oggetto                           */
/*       *-------------------------------------------------------*/
  KEY `idx_oiv_obj`
      (`obj_typ`, `obj_key`, `obj_dpz`),

/*       *-------------------------------------------------------*/
/*       * Ricerca voucher per segnale                           */
/*       *-------------------------------------------------------*/
  KEY `idx_oiv_sig`
      (`cod_sig`)

) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
