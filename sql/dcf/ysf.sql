/*    *==========================================================*/
/*    * Tabella [zsf] Spese per fatturazione fornitori     {dcf} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_spf,
                                                        cod_lng),
/*       *=======================================================*/
/*       * Numero spesa per fatturazione o per lo sconto         */
/*       *                                                       */
/*       * N.B.: I valori vengono cosi' utilizzati               */
/*       *                                                       */
/*       *       - 001..006 : Codici spesa per fatturazione      */
/*       *       - 101..102 : Sconti in chiusura fattura         */
/*       *-------------------------------------------------------*/
  num_spf                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice lingua                                         */
/*       *-------------------------------------------------------*/
  cod_lng                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione per video estesa, solo se lingua italiano */
/*       *-------------------------------------------------------*/
  des_ves                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione per video ridotta, solo se lingua italia- */
/*       * no                                                    */
/*       *-------------------------------------------------------*/
  des_vri                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione per stampa                                */
/*       *                                                       */
/*       * N.B. : se il codice lingua non e' l'italiano, il re-  */
/*       *        cord comporta solamente questo campo           */
/*       *-------------------------------------------------------*/
  des_stp                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo funzionamento spesa, solo se lingua italiano     */
/*       *                                                       */
/*       * - 01 : A percentuale fissa per tutti i fornitori cui  */
/*       *        viene addebitata la spesa                      */
/*       * - 02 : A percentuale variabile per ogni fornitore cui */
/*       *        viene addebitata la spesa, con la percentuale  */
/*       *        di spesa espressa nel record dati commerciali  */
/*       *        del fornitore                                  */
/*       * - 03 : Ad importo fisso per tutti i fornitori cui     */
/*       *        viene addebitata la spesa                      */
/*       * - 04 : Ad importo variabile per tutti i fornitori cui */
/*       *        viene addebitata la spesa, con l'importo della */
/*       *        spesa espresso nel record dati commerciali del */
/*       *        fornitore                                      */
/*       * - 05 : A seconda del fornitore, il quale esprimera'   */
/*       *        se la spesa :                                  */
/*       *        - non va' addebitata                           */
/*       *        - va' addebitata in percentuale                */
/*       *        - va' addebitata ad importo                    */
/*       *        e che inoltre esprimera' l'eventuale percen-   */
/*       *        tuale o l'eventuale importo                    */
/*       *                                                       */
/*       *  N.B.: Solo per spese in fatturazione                 */
/*       *-------------------------------------------------------*/
  tfu_spe                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Percentuale fissa per la spesa in caso che il tipo    */
/*       * funzionamento spesa sia pari a 01, oppure di default  */
/*       * per l'anagrafica dati commerciali fornitore nel caso  */
/*       * che il tipo funzionamento spesa sia pari a 02         */
/*       *                                                       */
/*       *  N.B.: Solo per spese in fatturazione                 */
/*       *-------------------------------------------------------*/
  per_spe                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Imponibile per la percentuale della spesa, solo se    */
/*       * il tipo funzionamento spesa e' 01 o 02 o 05           */
/*       * - 00 : Non significativo                              */
/*       * - 01 : Totale lordo                                   */
/*       * - 02 : Totale netto                                   */
/*       * - 03 : Su una combinazione delle voci seguenti        */
/*       * - 11 : Totale merci                                   */
/*       * - 12 : Totale servizi                                 */
/*       * - 13 : Totale imballi                                 */
/*       * - 14 : Totale libero 4                                */
/*       * - 15 : Totale libero 5                                */
/*       * - 16 : Totale libero 6                                */
/*       * - 17 : Totale libero 7                                */
/*       * - 18 : Totale libero 8                                */
/*       * - 19 : Totale extra                                   */
/*       *                                                       */
/*       *  N.B.: Solo per spese in fatturazione                 */
/*       *-------------------------------------------------------*/
  ibl_spe                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Totalizzatori da considerarsi per la determinazione   */
/*       * dell'imponibile per la percentuale della spesa        */
/*       *                                                       */
/*       * Solo se l'imponibile per la percentuale e' pari a 03  */
/*       *                                                       */
/*       * Per ogni totalizzatore deve essere specificato un va- */
/*       * lore di un carattere, dove :                          */
/*       * - N : Non concorre a formare l'imponibile             */
/*       * - S : Concorre a formare l'imponibile                 */
/*       *                                                       */
/*       * Il significato dei totalizzatori e' di tipo posizio-  */
/*       * nale, con la seguente codifica :                      */
/*       *                                                       */
/*       * 1 : Totale merci                                      */
/*       * 2 : Totale servizi                                    */
/*       * 3 : Totale imballi                                    */
/*       * 4 : Totale libero 4                                   */
/*       * 5 : Totale libero 5                                   */
/*       * 6 : Totale libero 6                                   */
/*       * 7 : Totale libero 7                                   */
/*       * 8 : Totale libero 8                                   */
/*       * 9 : Totale extra                                      */
/*       *                                                       */
/*       *  N.B.: Solo per spese in fatturazione                 */
/*       *-------------------------------------------------------*/
  ibt_sp1                   varchar (01)                not null,
  ibt_sp2                   varchar (01)                not null,
  ibt_sp3                   varchar (01)                not null,
  ibt_sp4                   varchar (01)                not null,
  ibt_sp5                   varchar (01)                not null,
  ibt_sp6                   varchar (01)                not null,
  ibt_sp7                   varchar (01)                not null,
  ibt_sp8                   varchar (01)                not null,
  ibt_sp9                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo fisso per la spesa in caso che il tipo fun-   */
/*       * zionamento spesa sia pari a 03, oppure di default per */
/*       * l'anagrafica dati commerciali fornitore nel caso che  */
/*       * il tipo funzionamento spesa sia pari a 04, solo se    */
/*       * lingua italiano                                       */
/*       *                                                       */
/*       *  N.B.: Solo per spese in fatturazione                 */
/*       *-------------------------------------------------------*/
  imp_spe                   bigint  (09)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice iva per la spesa. Se zero significa che la spe-*/
/*       * sa non e' soggetta ad una aliquota iva fissa, bensi'  */
/*       * che l'importo della spesa va' sventagliato in propor- */
/*       * zione agli altri imponibili di fattura                */
/*       *                                                       */
/*       *  N.B.: Valido anche per lo sconto                     */
/*       *-------------------------------------------------------*/
  civ_spe                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice contropartita per la spesa. Se zero significa  */
/*       * che la spesa non ha una contropartita fissa, bensi'   */
/*       * che l'importo della spesa va' sventagliato in propor- */
/*       * zione alle altre contropartite di fattura             */
/*       *                                                       */
/*       *  N.B.: Valido anche per lo sconto                     */
/*       *-------------------------------------------------------*/
  ccp_spe                   integer (07)                not null
