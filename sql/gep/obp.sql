/*    *==========================================================*/
/*    * Tabella [ddp] Distinte di presentazione            {gep} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tip_obp,
                                                        num_obp),
/*       *=======================================================*/
/*       * Tipo record                                           */
/*       *                                                       */
/*       * - 01 : Relativo alle scadenze debitori  in [sdb]      */
/*       *        per quanto riguarda la ragione sociale del-    */
/*       *        l'obbligato principale in caso di cessione     */
/*       *        paghero' cambiario da cliente                  */
/*       *                                                       */
/*       * - 51 : Relativo alle scadenze fornitori in [scf]      */
/*       *        per quanto riguarda la ragione sociale del-    */
/*       *        l'obbligato principale in caso di cessione     */
/*       *        paghero' cambiario a fornitore                 */
/*       *                                                       */
/*       * - 61 : Relativo alle scadenze fornitori in [scf]      */
/*       *        per quanto riguarda la ragione sociale del-    */
/*       *        la banca estera di appoggio in caso di bo-     */
/*       *        nifici bancari in valuta, per l'operazione     */
/*       *        di registrazione scadenza                      */
/*       *-------------------------------------------------------*/
  tip_obp                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero scadenza s.aa.nnnnnnnn                         */
/*       *-------------------------------------------------------*/
  num_obp                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Ragione sociale                                       */
/*       *-------------------------------------------------------*/
  rag_obp                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Indirizzo o filiale banca estera                      */
/*       *-------------------------------------------------------*/
  via_obp                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Localita'                                             */
/*       *-------------------------------------------------------*/
  loc_obp                   varchar (80)                not null
