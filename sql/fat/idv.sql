/*    *==========================================================*/
/*    * Tabella [idv] Inter-relazioni documenti di vendita {fat} */
/*    *                                                          */
/*    * Aggiornamento 16.04.2025                                 */
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
/*       *-------------------------------------------------------*/
/*       * Indice doc-level SORGENTE                             */
/*       * - accelera: WHERE (tip_dsr=? AND prt_dsr=?)            */
/*       * - utile per leggere anche il lato target e la data     */
/*       *-------------------------------------------------------*/
  KEY                       IDV_DSR                    (tip_dsr,
                                                        prt_dsr,
                                                        tip_dtr,
                                                        prt_dtr,
                                                        ide_dat),
/*       *-------------------------------------------------------*/
/*       * Indice doc-level TARGET                               */
/*       * - accelera: WHERE (tip_dtr=? AND prt_dtr=?)            */
/*       * - utile per leggere anche il lato source e la data     */
/*       *-------------------------------------------------------*/
  KEY                       IDV_DTR                    (tip_dtr,
                                                        prt_dtr,
                                                        tip_dsr,
                                                        prt_dsr,
                                                        ide_dat),
/*       *=======================================================*/
/*       * Tipo documento sorgente                               */
/*       *                                                       */
/*       * - 01 : Ordine cliente                         Vendite */
/*       * - 11 : Ordine di spedizione                   Vendite */
/*       * - 21 : Documento di trasporto                 Vendite */
/*       * - 31 : Fattura di vendita                     Vendite */
/*       * - 41 : Scadenza cliente ___ ATTUALMENTE NON GESTITE   */
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
/*       * - 01 : Ordine cliente                         Vendite */
/*       * - 11 : Ordine di spedizione                   Vendite */
/*       * - 21 : Documento di trasporto                 Vendite */
/*       * - 31 : Fattura di vendita                     Vendite */
/*       * - 41 : Scadenza cliente ___ ATTUALMENTE NON GESTITE   */
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
