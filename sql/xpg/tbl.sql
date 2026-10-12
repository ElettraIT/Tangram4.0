/*    *==========================================================*/
/*    * Tabella [tbl] Tabelle del database                 {xpg} */
/*    *                                                          */
/*    * Aggiornamento 15.09.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_tbl),
  KEY         ix_tbl_amb                               (amb_tbl,
                                                        sts_tbl),
  KEY         ix_tbl_are                               (are_tbl,
                                                        cod_tbl),
/*       *=======================================================*/
/*       * Codice tabella                                        */
/*       *-------------------------------------------------------*/
  cod_tbl                   varchar  (12)               not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione tabella                                   */
/*       *-------------------------------------------------------*/
  des_tbl                   varchar  (40)               not null,
/*       *-------------------------------------------------------*/
/*       * Area gestionale                                       */
/*       *-------------------------------------------------------*/
  are_tbl                   varchar  (04)               not null,
/*       *-------------------------------------------------------*/
/*       * Nome tabella                                          */
/*       *-------------------------------------------------------*/
  nom_tbl                   varchar  (04)               not null,
/*       *-------------------------------------------------------*/
/*       * Dipendenza tabella (tabella madre)                    */
/*       *-------------------------------------------------------*/
  dip_tbl                   varchar  (12)               not null,
/*       *-------------------------------------------------------*/
/*       * Percorso del file DDL                                 */
/*       *-------------------------------------------------------*/
  pth_tbl                   varchar  (40)               not null,
/*       *-------------------------------------------------------*/
/*       * Stato di gestione                                     */
/*       *                                                       */
/*       * - 'S' : Gestita                                       */
/*       * - 'A' : Da implementare o analizzare                  */
/*       * - 'N' : Non piu' gestita                              */
/*       *-------------------------------------------------------*/
  sts_tbl                   varchar  (01)               not null,
/*       *-------------------------------------------------------*/
/*       * Ambito : dove risiede la tabella                      */
/*       *                                                       */
/*       * - 'S' : Sistema  - istanza unica in [tgm]             */
/*       * - 'C' : Comune   - istanza unica in [fdb]             */
/*       * - 'A' : Azienda  - una per ogni database azienda      */
/*       *-------------------------------------------------------*/
  amb_tbl                   char     (01)               not null,
/*       *-------------------------------------------------------*/
/*       * Categoria di provisioning : come nasce la tabella     */
/*       *                                                       */
/*       * - 'S' : Standard di prodotto - seed da file csv       */
/*       * - 'M' : Base aziendale       - copia da modello       */
/*       * - 'A' : Specifica azienda    - nasce vuota            */
/*       * - 'V' : Movimenti            - nasce vuota            */
/*       *-------------------------------------------------------*/
  cat_tbl                   char     (01)               not null,
/*       *-------------------------------------------------------*/
/*       * File di seed (csv), se cat_tbl = 'S'                  */
/*       *-------------------------------------------------------*/
  sed_tbl                   varchar  (40)               not null,
/*       *-------------------------------------------------------*/
/*       * Versione di schema in cui la tabella e' comparsa      */
/*       *-------------------------------------------------------*/
  ver_tbl                   smallint unsigned           not null