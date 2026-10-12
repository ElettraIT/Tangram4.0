/*    *==========================================================*/
/*    * Tabella [ztv] Tipi di variante                     {dcp} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *                                                          */
/*    * N.B.: questo tabella non e' attualmente gestita          */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tip_var),
/*       *=======================================================*/
/*       * Tipo variante                                         */
/*       *-------------------------------------------------------*/
  tip_var                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione variante                                  */
/*       *-------------------------------------------------------*/
  des_var                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo funzionamento variante                           */
/*       *                                                       */
/*       * N.B.: questo campo non e' attualmente gestito         */
/*       *-------------------------------------------------------*/
  tip_fun                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Parametri liberi variante                             */
/*       *                                                       */
/*       * N.B.: questo campo non e' attualmente gestito         */
/*       *-------------------------------------------------------*/
  prm_lb1                   varchar (03)                not null,
  prm_lb2                   varchar (03)                not null,
  prm_lb3                   varchar (03)                not null
