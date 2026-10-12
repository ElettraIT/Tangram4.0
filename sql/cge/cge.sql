/*    *==========================================================*/
/*    * Tabella [cge] Personalizzazioni primanota          {cge} */
/*    *                                                          */
/*    * Parametri di configurazione della contabilita' generale, */
/*    * in forma codice / valore.                                */
/*    *                                                          */
/*    * Aggiornamento 15.09.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_cge),
/*       *=======================================================*/
/*       * Codice personalizzazione                              */
/*       *-------------------------------------------------------*/
  cod_cge                   varchar  (24)               not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione personalizzazione                         */
/*       *-------------------------------------------------------*/
  des_cge                   varchar  (120)              not null,
/*       *-------------------------------------------------------*/
/*       * Valore numerico                                       */
/*       *-------------------------------------------------------*/
  val_num                   int      (11)               not null default 0,
/*       *-------------------------------------------------------*/
/*       * Valore alfanumerico                                   */
/*       *                                                       */
/*       * NULL quando la personalizzazione e' solo numerica.    */
/*       *-------------------------------------------------------*/
  val_alf                   varchar  (120)                  null default null
