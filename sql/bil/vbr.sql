/*    *==========================================================*/
/*    * Tabella [vbr] Voci di bilancio : righe                   */
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
                                                        vdb_lv6,
                                                        num_prg),
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
/*       * Numero progressivo elemento                           */
/*       *-------------------------------------------------------*/
  num_prg                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo elemento                                         */
/*       * - 'V' : Voce di bilancio                              */
/*       * - 'S' : Sottoconto contabile                          */
/*       *-------------------------------------------------------*/
  tip_elm                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di saldo                                         */
/*       * - 'D' : Dare                                          */
/*       * - 'A' : Avere                                         */
/*       *-------------------------------------------------------*/
  tip_sld                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo operazione da effettuare sull'elemento           */
/*       * - '+' : Addizione                                     */
/*       * - '-' : Sottrazione                                   */
/*       *                                                       */
/*       * N.B.: solo se tipo elemento 01 : Voce di bilancio     */
/*       *-------------------------------------------------------*/
  tip_ope                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Codici voce di risultato di livello                   */
/*       *-------------------------------------------------------*/
  vrs_lv1                   integer (05)                not null,
  vrs_lv2                   integer (05)                not null,
  vrs_lv3                   integer (05)                not null,
  vrs_lv4                   integer (05)                not null,
  vrs_lv5                   integer (05)                not null,
  vrs_lv6                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice sottoconto contabile                           */
/*       *                                                       */
/*       * N.B.: solo se tipo elemento 02 : Sottoconto           */
/*       *-------------------------------------------------------*/
  cod_stc                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Note relative all'elemento                            */
/*       *                                                       */
/*       * N.B.: Campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  not_elm                   varchar (40)                not null