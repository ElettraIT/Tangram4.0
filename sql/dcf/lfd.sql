/*    *==========================================================*/
/*    * Tabella [lfd] Listini storici fornitori            {dcf} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tip_rec,
                                                        cod_dcf,
                                                        sgl_vlt,
                                                        tip_mag,
                                                        num_mag,
                                                        fda_pif,
                                                        dva_fin),
/*       *=======================================================*/
/*       * Tipo record                                           */
/*       *                                                       */
/*       * - 01 : Listini di acquisto prodotti per fornitore     */
/*       *-------------------------------------------------------*/
  tip_rec                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice fornitore                                      */
/*       *-------------------------------------------------------*/
  cod_dcf                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Sigla valuta per il prezzo                            */
/*       *-------------------------------------------------------*/
  sgl_vlt                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Decimali valuta per il prezzo                         */
/*       *-------------------------------------------------------*/
  dec_vlt                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
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
/*       * Formato di acquisizione presso il fornitore           */
/*       *-------------------------------------------------------*/
  fda_pif                   varchar (20)                not null,
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
/*       * Data di validita' iniziale                            */
/*       *                                                       */
/*       * N.B.: Campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  dva_ini                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Data di validita' finale                              */
/*       *                                                       */
/*       * N.B.: Data di storicizzazione del listino, ovvero     */
/*       *       data fino alla quale valgono prezzi e sconti    */
/*       *-------------------------------------------------------*/
  dva_fin                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Data di validita' finale in formato 'reverse'         */
/*       *                                                       */
/*       * Si ottiene con il seguente calcolo:                   */
/*       * rf-lfd-dvf-rev = (99999999-(19000000+rf-lfd-dva-fin)) */
/*       *                                                       */
/*       * Se rf-lfd-dva-fin vale zero, questo campo viene for-  */
/*       * zato al valore 99999999                               */
/*       *                                                       */
/*       * E' da osservare pertanto che record senza data di va- */
/*       * lidita' finale avrebbero qui il valore 99999999       */
/*       *                                                       */
/*       * In ogni caso un prezzo storicizzato non puo' mancare  */
/*       * mai della data di validita' finale                    */
/*       *-------------------------------------------------------*/
  dvf_rev                   bigint  (11)                not null