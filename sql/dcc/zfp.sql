/*    *==========================================================*/
/*    * Tabella [zfp] Forme di pagamento clienti           {dcc} */
/*    *                                                          */
/*    * Aggiornamento 30.07.2025                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_fop),
/*       *=======================================================*/
/*       * Codice forma di pagamento                             */
/*       *-------------------------------------------------------*/
  cod_fop                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice mnemonico                                      */
/*       *-------------------------------------------------------*/
  cod_mne                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione forma di pagamento                        */
/*       *-------------------------------------------------------*/
  des_fop                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Sconto in fatturazione associato al pagamento         */
/*       *-------------------------------------------------------*/
  sco_fat                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Codice 1 di pagamento                                 */
/*       *-------------------------------------------------------*/
/*           *---------------------------------------------------*/
/*           * Codice                                            */
/*           *---------------------------------------------------*/
  cod_pg1                   integer (03)                not null,
/*           *---------------------------------------------------*/
/*           * Tipo di ammontare su cui agisce il codice di pa-  */
/*           * gamento                                           */
/*           *                                                   */
/*           * - 01 : Il totale documento                        */
/*           * - 02 : Il totale imponibile                       */
/*           * - 03 : Una percentuale del totale documento       */
/*           * - 04 : Una percentuale del totale imponibile      */
/*           * - 05 : Il totale Iva                              */
/*           * - 06 : Solo una quota a forfait                   */
/*           * - 07 : Totale Iva piu' quota a forfait            */
/*           * - 08 : Totale documento  meno quota a forfait     */
/*           * - 09 : Totale imponibile meno quota a forfait     */
/*           * - 10 : Il residuo per il totale documento         */
/*           * - 11 : Il residuo per il totale imponibile        */
/*           *---------------------------------------------------*/
  tip_am1                   tinyint (02)                not null,
/*           *---------------------------------------------------*/
/*           * Percentuale del totale documento o dell'imponibi- */
/*           * le, obbligatoria se tipo ammontare su percentuale */
/*           *---------------------------------------------------*/
  per_to1                   decimal (05,02)             not null,
/*           *---------------------------------------------------*/
/*           * Segnale di diminuzione dell'ammontare per l'im-   */
/*           * porto dell'acconto                                */
/*           *                                                   */
/*           * - 01 : No, nessuna diminuzione dell'ammontare     */
/*           * - 02 : Si, l'ammontare viene diminuito dell'ac-   */
/*           *        conto                                      */
/*           *---------------------------------------------------*/
  dim_ac1                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice 2 di pagamento                                 */
/*       *-------------------------------------------------------*/
/*           *---------------------------------------------------*/
/*           * Codice                                            */
/*           *---------------------------------------------------*/
  cod_pg2                   integer (03)                not null,
/*           *---------------------------------------------------*/
/*           * Tipo di ammontare su cui agisce il codice di pa-  */
/*           * gamento                                           */
/*           *                                                   */
/*           * - 01 : Il totale documento                        */
/*           * - 02 : Il totale imponibile                       */
/*           * - 03 : Una percentuale del totale documento       */
/*           * - 04 : Una percentuale del totale imponibile      */
/*           * - 05 : Il totale Iva                              */
/*           * - 06 : Solo una quota a forfait                   */
/*           * - 07 : Totale Iva piu' quota a forfait            */
/*           * - 08 : Totale documento  meno quota a forfait     */
/*           * - 09 : Totale imponibile meno quota a forfait     */
/*           * - 10 : Il residuo per il totale documento         */
/*           * - 11 : Il residuo per il totale imponibile        */
/*           *---------------------------------------------------*/
  tip_am2                   tinyint (02)                not null,
/*           *---------------------------------------------------*/
/*           * Percentuale del totale documento o dell'imponibi- */
/*           * le, obbligatoria se tipo ammontare su percentuale */
/*           *---------------------------------------------------*/
  per_to2                   decimal (05,02)             not null,
/*           *---------------------------------------------------*/
/*           * Segnale di diminuzione dell'ammontare per l'im-   */
/*           * porto dell'acconto                                */
/*           *                                                   */
/*           * - 01 : No, nessuna diminuzione dell'ammontare     */
/*           * - 02 : Si, l'ammontare viene diminuito dell'ac-   */
/*           *        conto                                      */
/*           *---------------------------------------------------*/
  dim_ac2                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice 3 di pagamento                                 */
/*       *-------------------------------------------------------*/
/*           *---------------------------------------------------*/
/*           * Codice                                            */
/*           *---------------------------------------------------*/
  cod_pg3                   integer (03)                not null,
/*           *---------------------------------------------------*/
/*           * Tipo di ammontare su cui agisce il codice di pa-  */
/*           * gamento                                           */
/*           *                                                   */
/*           * - 01 : Il totale documento                        */
/*           * - 02 : Il totale imponibile                       */
/*           * - 03 : Una percentuale del totale documento       */
/*           * - 04 : Una percentuale del totale imponibile      */
/*           * - 05 : Il totale Iva                              */
/*           * - 06 : Solo una quota a forfait                   */
/*           * - 07 : Totale Iva piu' quota a forfait            */
/*           * - 08 : Totale documento  meno quota a forfait     */
/*           * - 09 : Totale imponibile meno quota a forfait     */
/*           * - 10 : Il residuo per il totale documento         */
/*           * - 11 : Il residuo per il totale imponibile        */
/*           *---------------------------------------------------*/
  tip_am3                   tinyint (02)                not null,
/*           *---------------------------------------------------*/
/*           * Percentuale del totale documento o dell'imponibi- */
/*           * le, obbligatoria se tipo ammontare su percentuale */
/*           *---------------------------------------------------*/
  per_to3                   decimal (05,02)             not null,
/*           *---------------------------------------------------*/
/*           * Segnale di diminuzione dell'ammontare per l'im-   */
/*           * porto dell'acconto                                */
/*           *                                                   */
/*           * - 01 : No, nessuna diminuzione dell'ammontare     */
/*           * - 02 : Si, l'ammontare viene diminuito dell'ac-   */
/*           *        conto                                      */
/*           *---------------------------------------------------*/
  dim_ac3                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/no possibilita' di intervento manuale sulle sca-   */
/*       * denze in fase di emissione da fattura                 */
/*       *                                                       */
/*       * - ' ' o 'N' : No                                      */
/*       * - 'S'       : Si                                      */
/*       *-------------------------------------------------------*/
  snx_ims                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/no forma di pagamento di tipo contrassegno         */
/*       *                                                       */
/*       * - ' ' o 'N' : No                                      */
/*       * - 'S'       : Si                                      */
/*       *-------------------------------------------------------*/
  snx_cts                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
