/*    *==========================================================*/
/*    * Tabella [ccc] Anagrafica controllo crediti         {gep} */
/*    *                                                          */
/*    * Aggiornamento 07.02.2014                                 */
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
/*       * Emissione di sollecito al cliente                     */
/*       *                                                       */
/*       * - 00 : Si, sempre                                     */
/*       * - 01 : Si, ma solo fino al 1. livello, e non oltre    */
/*       * - 02 : Si, ma solo fino al 2. livello, e non oltre    */
/*       * - 99 : No, mai                                        */
/*       *-------------------------------------------------------*/
  emi_slc                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di testo di sollecito per il cliente             */
/*       *                                                       */
/*       * - 00 : Standard, a seconda del livello di sollecito   */
/*       * - 01 : Standard, ma sempre con il testo per il 1. li- */
/*       *        vello                                          */
/*       * - 02 : Standard, ma sempre con il testo per il 2. li- */
/*       *        vello                                          */
/*       * - 03 : Standard, ma sempre con il testo per il 3. li- */
/*       *        vello                                          */
/*       * - 99 : Personalizzato                                 */
/*       *-------------------------------------------------------*/
  tdt_slc                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice tipo sollecito personalizzato per il cliente   */
/*       * per quanto riguarda il sollecito di 1. livello        */
/*       *                                                       */
/*       * N.B. : Solo se tipo di testo per il sollecito 99      */
/*       *-------------------------------------------------------*/
  tso_l01                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice tipo sollecito personalizzato per il cliente   */
/*       * per quanto riguarda il sollecito di 2. livello        */
/*       *                                                       */
/*       * N.B. : Solo se tipo di testo per il sollecito 99      */
/*       *-------------------------------------------------------*/
  tso_l02                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice tipo sollecito personalizzato per il cliente   */
/*       * per quanto riguarda il sollecito di 3. livello        */
/*       *                                                       */
/*       * N.B. : Solo se tipo di testo per il sollecito 99      */
/*       *-------------------------------------------------------*/
  tso_l03                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Tasso di interesse da applicare al cliente, in caso   */
/*       * di sollecito con applicazione del tasso di interesse. */
/*       *                                                       */
/*       * Solo se diverso da zero questo prevale sul tasso di   */
/*       * interesse associato al tipo di sollecito              */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  tas_api                   decimal (06,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Fido massimo concesso al cliente                      */
/*       *                                                       */
/*       * N.B. : a seconda della personalizzazione, puo' essere */
/*       *        espresso in migliaia                           */
/*       *-------------------------------------------------------*/
  max_fid                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di assegnazione del fido                         */
/*       *-------------------------------------------------------*/
  dat_fid                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Si/No assicurazione a copertura del credito           */
/*       *                                                       */
/*       *  - Spaces : Non richiesta                             */
/*       *  - S      : Accordata                                 */
/*       *  - N      : Rifiutata                                 */
/*       *-------------------------------------------------------*/
  snx_acr                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo assicurazione a copertura del credito         */
/*       *-------------------------------------------------------*/
  imp_acr                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Data aggiornamento assicurazione a copertura credito  */
/*       *-------------------------------------------------------*/
  dat_acr                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Data scadenza assicurazione a copertura credito       */
/*       *-------------------------------------------------------*/
  scd_acr                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
