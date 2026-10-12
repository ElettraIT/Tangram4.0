/*    *==========================================================*/
/*    * Tabella [vbt] Voci di bilancio : testate                 */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tip_bil,
                                                        vdb_lv1,
                                                        vdb_lv2,
                                                        vdb_lv3,
                                                        vdb_lv4,
                                                        vdb_lv5,
                                                        vdb_lv6),
/*       *=======================================================*/
/*       * Tipo di bilancio interessato per la voce              */
/*       *-------------------------------------------------------*/
  tip_bil                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codici voce di bilancio di livello                    */
/*       *-------------------------------------------------------*/
  vdb_lv1                   integer (05)                not null,
  vdb_lv2                   integer (05)                not null,
  vdb_lv3                   integer (05)                not null,
  vdb_lv4                   integer (05)                not null,
  vdb_lv5                   integer (05)                not null,
  vdb_lv6                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero di livello voce di bilancio                    */
/*       *-------------------------------------------------------*/
  liv_vdb                   integer (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Mnemonico voce di bilancio                            */
/*       *-------------------------------------------------------*/
  mne_vdb                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione voce di bilancio                          */
/*       *-------------------------------------------------------*/
  des_vdb                   varchar (250)               not null,
/*       *-------------------------------------------------------*/
/*       * Tipo voce di bilancio                                 */
/*       *                                                       */
/*       * - 01 : Ulteriormente suddiviso                        */
/*       * - 02 : Di ultimo livello                              */
/*       * - 03 : Di risultato                                   */
/*       * - 04 : Di separata indicazione                        */
/*       *-------------------------------------------------------*/
  tip_vdb                   integer (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No inversione del segno                            */
/*       *-------------------------------------------------------*/
  snx_isg                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No stampa del totale voce di bilancio              */
/*       *-------------------------------------------------------*/
  snx_tot                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Dicitura per totale voce di bilancio                  */
/*       *-------------------------------------------------------*/
  dic_tot                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Spazio di separazione da elemento precedente          */
/*       *                                                       */
/*       * - 01 : Nessuno spazio                                 */
/*       * - 02 : Un'interlinea                                  */
/*       * - 03 : Due interlinee                                 */
/*       * - 04 : Tre interlinee                                 */
/*       * - 09 : Salto pagina                                   */
/*       *-------------------------------------------------------*/
  spz_sep                   integer (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
