/*    *==========================================================*/
/*    * Tabella [aaf] Dati di acquisto prodotti            {dcf} */
/*    *                                                          */
/*    * Vedi [aap] per scaglioni prezzo                          */
/*    *                                                          */
/*    * Aggiornamento 02.05.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tip_mag,
                                                        num_mag,
                                                        cod_dcf,
                                                        fda_pif),
/*       *=======================================================*/
/*       * Tipo codice di magazzino                              */
/*       *                                                       */
/*       * - 01 : Prodotti di vendita                            */
/*       * - 03 : Materie prime                                  */
/*       * - 04 : Materiali vari                                 */
/*       *-------------------------------------------------------*/
  tip_mag                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice prodotto numerico                              */
/*       *-------------------------------------------------------*/
  num_mag                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice fornitore                                      */
/*       *-------------------------------------------------------*/
  cod_dcf                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Formato di acquisizione presso il fornitore           */
/*       *-------------------------------------------------------*/
  fda_pif                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice prodotto per il fornitore                      */
/*       *-------------------------------------------------------*/
  cop_sfn                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione prodotto per il fornitore                 */
/*       *                                                       */
/*       * N.B. : Se il campo e' tutto a spazi, per convenzione  */
/*       *        zione significa che la descrizione prodotto    */
/*       *        deve essere presa dall'anagrafica del codice   */
/*       *        magazzino                                      */
/*       *-------------------------------------------------------*/
  dep_sfn                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Presenza della descrizione alternativa di max 400     */
/*       * caratteri per il fornitore                            */
/*       *                                                       */
/*       * - 0 : No                                              */
/*       * - 1 : Si                                              */
/*       *                                                       */
/*       * N.B. : Se il campo precedente indica che la descri-   */
/*       *        zione deve essere presa dall'anagrafica del    */
/*       *        codice magazzino, questo campo non e' signi-   */
/*       *        ficativo                                       */
/*       *-------------------------------------------------------*/
  xdp_sfn                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No trasformazione unita' di misura                 */
/*       *                                                       */
/*       *   - N : No, nessuna trasformazione                    */
/*       *                                                       */
/*       *   - S : Si trasformazione, il fornitore utilizza per  */
/*       *         la vendita una diversa unita' di misura       */
/*       *         rispetto a quella utilizzata nella gestione   */
/*       *         del magazzino, pertanto quantita' e prezzo    */
/*       *         validi per gli ordini si riferiscono all'uni- */
/*       *         ta' di misura adottata dal fornitore          */
/*       *                                                       */
/*       *   - P : Si trasformazione, il fornitore utilizza per  */
/*       *         la vendita una diversa unita' di misura       */
/*       *         rispetto a quella utilizzata nella gestione   */
/*       *         del magazzino, pero' accetta ordini nella     */
/*       *         unita' di misura utilizzata nella gestione    */
/*       *         del magazzino, fermo restando pero' che fat-  */
/*       *         turera' in termini della sua unita' di misura */
/*       *-------------------------------------------------------*/
  snx_tum                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Unita' di misura utilizzata dal fornitore             */
/*       *                                                       */
/*       * Solo se snx-tum pari a 'S' o 'P'                      */
/*       *-------------------------------------------------------*/
  umf_tum                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero decimali utilizzato dal fornitore per esprime- */
/*       * re la quantita' nella sua unita' di misura            */
/*       *                                                       */
/*       * Solo se snx-tum pari a 'S' o 'P'                      */
/*       *-------------------------------------------------------*/
  nde_tum                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Coefficiente moltiplicatore, per cui va' moltiplicata */
/*       * la quantita' espressa nell'unita' di misura del       */
/*       * fornitore, per ridurla alla unita' di misura utiliz-  */
/*       * zata da noi                                           */
/*       *                                                       */
/*       * Solo se snx-tum pari a 'S' o 'P'                      */
/*       *-------------------------------------------------------*/
  cmo_tum                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Coefficiente divisore, per cui va' divisa la          */
/*       * quantita' espressa nell'unita' di misura del          */
/*       * fornitore, per ridurla alla unita' di misura          */
/*       * utilizzata da noi                                     */
/*       *                                                       */
/*       * Solo se snx-tum pari a 'S' o 'P'                      */
/*       *-------------------------------------------------------*/
  cdi_tum                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza del fornitore                       */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  dpz_dcf                   varchar (04)                not null,
/*       *-------------------------------------------------------*/
/*       * Annotazioni                                           */
/*       *                                                       */
/*       * N.B. : Campo sostituito da record [pdx]               */
/*       *-------------------------------------------------------*/
  ann_not                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Modalita' di espressione tempo di consegna            */
/*       *-------------------------------------------------------*/
  mde_tdc                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Tempo di consegna pattuito con il fornitore,          */
/*       * espresso nella modalita' stabilita                    */
/*       *-------------------------------------------------------*/
  tmp_cns                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Sigla valuta per il prezzo d'acquisto                 */
/*       *-------------------------------------------------------*/
  sgl_vlt                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Decimali valuta per il prezzo d'acquisto              */
/*       *-------------------------------------------------------*/
  dec_vlt                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Decimali per il prezzo d'acquisto                     */
/*       *-------------------------------------------------------*/
  dec_prz                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo prezzo di acquisto                               */
/*       *                                                       */
/*       * - 01 : Specifico per l'acquisto                       */
/*       * - 02 : Stesso prezzo listino base di vendita          */
/*       *                                                       */
/*       * N.B. : Se tipo codice di magazzino 03 (materie prime) */
/*       *        il campo viene forzato al valore 01            */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  tip_prz                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Lotto di acquisto                                     */
/*       *-------------------------------------------------------*/
  lot_acq                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di applicazione prezzo e sconti                  */
/*       *                                                       */
/*       * - 01 : Indipendentemente dalla quantita'              */
/*       * - 02 : A seconda della quantita'                      */
/*       *-------------------------------------------------------*/
  tap_pes                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data aggiornamento prezzi d'acquisto e sconti         */
/*       * pattuiti con il fornitore                             */
/*       *-------------------------------------------------------*/
  uda_pes                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Valuta di riferimento in caso di legame valutario     */
/*       *                                                       */
/*       * - Sigla                                               */
/*       * - Decimali                                            */
/*       * - Tipo cambio                                         */
/*       * - Cambio                                              */
/*       * - Percentuale di tolleranza                           */
/*       *                                                       */
/*       * N.B.: Gestita solo in presenza di apposita personal.  */
/*       *-------------------------------------------------------*/
  lgv_vlt                   varchar (03)                not null,
  lgv_dcv                   tinyint (01)                not null,
  lgv_tdc                   varchar (01)                not null,
  lgv_cdc                   decimal (12,05)             not null,
  lgv_pdt                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Prezzo di acquisto                                    */
/*       *-------------------------------------------------------*/
  prz_acq                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * % di sconto                                           */
/*       *-------------------------------------------------------*/
  per_sr1                   decimal (05,02)             not null,
  per_sr2                   decimal (05,02)             not null,
  per_sr3                   decimal (05,02)             not null,
  per_sr4                   decimal (05,02)             not null,
  per_sr5                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * % di maggiorazione del prezzo di acquisto             */
/*       *                                                       */
/*       * N.B.: Gestita solo in presenza di apposita personal.  */
/*       *-------------------------------------------------------*/
  per_mpa                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * % di ricarico sul prezzo netto di acquisto            */
/*       *                                                       */
/*       * N.B.: Gestita solo in presenza di apposita personal.  */
/*       *-------------------------------------------------------*/
  per_ric                   decimal (06,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
