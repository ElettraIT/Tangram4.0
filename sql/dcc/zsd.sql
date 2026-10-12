/*    *==========================================================*/
/*    * Tabella [zsd] Sconti in chiusura per documenti     {dcc} */
/*    *                                                          */
/*    * Aggiornamento 20.07.2007                                 */
/*    *                                                          */
/*    * ___ VALUTARE SOPPRESSIONE ___                            */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tip_doc,
                                                        tmo_doc),
/*       *=======================================================*/
/*       * Tipo di documento                                     */
/*       *                                                       */
/*       * - 01 : Fattura                                        */
/*       * - 11 : Bolla                                          */
/*       * - 21 : Ordine                                         */
/*       *                                                       */
/*       * N.B. : attualmente gestito solo il tipo '01'          */
/*       *-------------------------------------------------------*/
  tip_doc                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice tipo movimento                                 */
/*       *-------------------------------------------------------*/
  tmo_doc                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Totali da considerare per la determinazione degli     */
/*       * scaglioni di sconto                                   */
/*       *                                                       */
/*       * - 01 : Totale merce                                   */
/*       * - 02 : Totale servizi                                 */
/*       * - 03 : Totale imballi                                 */
/*       * - 04 : Totale libero 4                                */
/*       * - 05 : Totale libero 5                                */
/*       * - 06 : Totale libero 6                                */
/*       * - 07 : Totale libero 7                                */
/*       * - 08 : Totale libero 8                                */
/*       * - 09 : Totale extra                                   */
/*       *                                                       */
/*       * N.B. : campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  tot_s01                   varchar (01)                not null,
  tot_s02                   varchar (01)                not null,
  tot_s03                   varchar (01)                not null,
  tot_s04                   varchar (01)                not null,
  tot_s05                   varchar (01)                not null,
  tot_s06                   varchar (01)                not null,
  tot_s07                   varchar (01)                not null,
  tot_s08                   varchar (01)                not null,
  tot_s09                   varchar (01)                not null
  