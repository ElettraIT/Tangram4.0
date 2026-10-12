/*    *==========================================================*/
/*    * Tabella [vlt] Cambi valuta                         {dcc} */
/*    *                                                          */
/*    * Aggiornamento 22.10.2025                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (dat_vlt,
                                                        sgl_vlt),
/*       *=======================================================*/
/*       * Data rilevazione cambio                               */
/*       *-------------------------------------------------------*/
  dat_vlt                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Data rilevazione cambio in formato 'reverse'          */
/*       *                                                       */
/*       * Si ottiene con il seguente calcolo:                   */
/*       * rf-zlv-dat-rev = (99999999-(19000000+rf-vlt-dat-vlt)) */
/*       *                                                       */
/*       * Se rf-vlt-dat-vlt vale zero, questo campo viene for-  */
/*       * zato al valore 99999999                               */
/*       *                                                       */
/*       * E' da osservare pertanto che record senza data di ri- */
/*       * levazione cambio avranno qui il valore 99999999       */
/*       *-------------------------------------------------------*/
  dat_rev                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Sigla internazionale della valuta                     */
/*       *                                                       */
/*       * Nota : Per le lire italiane deve essere utilizzato il */
/*       *        codice 'LIT', che deve comunque essere presen- */
/*       *        te nella tabella valute.                       */
/*       *-------------------------------------------------------*/
  sgl_vlt                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo coefficiente di cambio                           */
/*       *                                                       */
/*       * - '/' : Divisore                                      */
/*       * - '*' : Moltiplicatore                                */
/*       *-------------------------------------------------------*/
  tdc_vlt                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Coefficiente di cambio ufficiale                      */
/*       *-------------------------------------------------------*/
  cdc_vlt                   decimal (18,06)             not null