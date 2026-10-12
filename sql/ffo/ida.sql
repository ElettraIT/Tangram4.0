/*    *==========================================================*/
/*    * Tabella [ida] Inter-relazioni documenti acquisto   {ffo} */
/*    *                                                          */
/*    * Aggiornamento 24.04.2025                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi (SRGTRG)                     */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tip_dsr,
                                                        prt_dsr,
                                                        prg_dsr,
                                                        tip_dtr,
                                                        prt_dtr,
                                                        prg_dtr),
/*       *=======================================================*/
/*       * Tipo documento sorgente                               */
/*       *                                                       */
/*       * - 01 : Ordine fornitore                      Acquisti */
/*       * - 11 : Conferma ordine    ___ ATTUALMENTE NON GESTITE */
/*       * - 21 : Documento di carico                   Acquisti */
/*       * - 31 : Fattura di acquisto                   Acquisti */
/*       * - 41 : Scadenze fornitore ___ ATTUALMENTE NON GESTITE */
/*       *-------------------------------------------------------*/
  tip_dsr                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativo documento sorgente                     */
/*       *-------------------------------------------------------*/
  prt_dsr                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativo documento sorgente - riga              */
/*       *-------------------------------------------------------*/
  prg_dsr                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo documento target                                 */
/*       *                                                       */
/*       * - 01 : Ordine fornitore                      Acquisti */
/*       * - 11 : Conferma ordine    ___ ATTUALMENTE NON GESTITE */
/*       * - 21 : Documento di carico                   Acquisti */
/*       * - 31 : Fattura di acquisto                   Acquisti */
/*       * - 41 : Scadenze fornitore ___ ATTUALMENTE NON GESTITE */
/*       *-------------------------------------------------------*/
  tip_dtr                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativo documento target                       */
/*       *-------------------------------------------------------*/
  prt_dtr                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativo documento target - riga                */
/*       *-------------------------------------------------------*/
  prg_dtr                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null
