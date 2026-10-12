/*    *==========================================================*/
/*    * Tabella [zcc] Codici causali contabili             {cge} */
/*    *                                                          */
/*    * Aggiornamento 11.11.2025                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_cau),
/*       *=======================================================*/
/*       * Codice causale                                        */
/*       *-------------------------------------------------------*/
  cod_cau                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione causale                                   */
/*       *-------------------------------------------------------*/
  des_cau                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale si/no movimento di bilancio della causale     */
/*       *                                                       */
/*       * - S : Si, movimento di rettifica al bilancio          */
/*       * - N : No, movimento di contabilita' ordinaria         */
/*       * - X : Movimento di chiusura e apertura bilancio       */
/*       *-------------------------------------------------------*/
  snx_mob                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo movimento iva                                    */
/*       *                                                       */
/*       * - 1 : Dare cliente Italia o Estero non CEE            */
/*       * - 2 : Avere cliente Italia o Estero non CEE           */
/*       * - 3 : Corrispettivi Italia o Estero non CEE           */
/*       *                                                       */
/*       * - 4 : Dare fornitore Italia o Estero non CEE          */
/*       * - 5 : Avere fornitore Italia o Estero non CEE         */
/*       * - 6 : Bolla doganale Estero non CEE                   */
/*       *                                                       */
/*       * - A : Dare cliente Estero CEE                         */
/*       * - B : Avere cliente Estero CEE                        */
/*       *                                                       */
/*       * - D : Dare fornitore Estero CEE                       */
/*       * - E : Avere fornitore Estero CEE                      */
/*       *-------------------------------------------------------*/
  tip_iva                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Mnemonico                                             */
/*       *-------------------------------------------------------*/
  cod_mne                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di documento                                     */
/*       *                                                       */
/*       * - ' ' : Non significativo                             */
/*       * - 'F' : Fattura                                       */
/*       * - 'S' : Fattura CEE servizi                           */
/*       * - 'M' : Fattura CEE merce                             */
/*       * - 'D' : Nota di debito                                */
/*       * - 'C' : Nota di credito                               */
/*       *                                                       */
/*       * N.B.: solo per causali Iva                            */
/*       *-------------------------------------------------------*/
  tip_doc                   varchar (01)                not null
