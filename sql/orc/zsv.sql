/*    *==========================================================*/
/*    * Tabella [zsv] Regole spese condizioni vendita      {orc} */
/*    *                                                          */
/*    * TABELLA SPERIMENTALE PER COLLEGARE LE CONDIZIONI DI      */
/*    * VENDITA ALLE SPESE (1..6)                                */
/*    *                                                          */
/*    * Aggiornamento 12.10.2026                                 */
/*    *----------------------------------------------------------*/
create table zsv
(
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_cdv,
                                                        num_spf),
/*       *=======================================================*/
/*       * Codice condizione di vendita                          */
/*       *                                                       */
/*       * Corrisponde a [zcv].cod_cdv                           */
/*       *-------------------------------------------------------*/
  `cod_cdv`                 varchar (3)     NOT NULL
                                            COMMENT 'Codice CDV',
/*       *=======================================================*/
/*       * Numero spesa per fatturazione o per lo sconto         */
/*       *                                                       */
/*       * N.B.: I valori vengono cosi' utilizzati               */
/*       *                                                       */
/*       *       - 001..006 : Codici spesa per fatturazione      */
/*       *       - 101..102 : Sconti in chiusura fattura         */
/*       *-------------------------------------------------------*/
  num_spf                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo regola                                           */
/*       *                                                       */
/*       * N.B. : default '04'                                   */
/*       *-------------------------------------------------------*/
    tip_reg                 char(2)         not null default '04',
/*       *-------------------------------------------------------*/
/*       * Flag Si/No attiva                                     */
/*       *                                                       */
/*       * N.B. : default 'S'                                    */
/*       *-------------------------------------------------------*/
    flg_att                 char(1)         not null default 'S',
);

create index zsv_i1
    on zsv (num_spf);
