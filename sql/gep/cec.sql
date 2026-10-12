/*    *==========================================================*/
/*    * Tabella [cec] Controllo esposizione clienti        {gep} */
/*    *                                                          */
/*    * Aggiornamento 13.07.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_cli),
/*       *=======================================================*/
/*       * Codice cliente                                        */
/*       *                                                       */
/*       * Nota : Le informazioni per il controllo del credito   */
/*       *        cliente sono valide sia per il cliente         */
/*       *        principale che per le sue dipendenze           */
/*       *-------------------------------------------------------*/
  cod_cli                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Progressivi cliente                                   */
/*       *                                                       */
/*       * - 01 : Progressivo ordini inevasi                     */
/*       * - 02 : Progressivo ordini di spedizione inevasi       */
/*       * - 03 : Progressivo bolle da fatturare                 */
/*       * - 04 : Progressivo scadenze aperte                    */
/*       * - 05 : (libero)                                       */
/*       * - 06 : (libero)                                       */
/*       *-------------------------------------------------------*/
  prg-v01                   bigint  (13)                not null,
  prg-v02                   bigint  (13)                not null,
  prg-v03                   bigint  (13)                not null,
  prg-v04                   bigint  (13)                not null,
  prg-v05                   bigint  (13)                not null,
  prg-v06                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Data scadenza assicurazione a copertura credito       */
/*       *-------------------------------------------------------*/
  dat_agg                   date                        not null
