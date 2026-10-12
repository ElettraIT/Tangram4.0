/*    *==========================================================*/
/*    * Tabella [ivp] Progressivi Iva per aliquota               */
/*    *                                                          */
/*    * Aggiornamento 15.09.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (ann_ivp,
                                                        mes_ivp,
                                                        cod_iva),
/*       *=======================================================*/
/*       * Anno di esercizio                                     */
/*       *-------------------------------------------------------*/
  ann_ivp     integer (04)  not null COMMENT 'Anno',
/*       *-------------------------------------------------------*/
/*       * Mese di esercizio                                     */
/*       *-------------------------------------------------------*/
  mes_ivp     tinyint (02)  not null COMMENT 'Mese',
/*       *-------------------------------------------------------*/
/*       * Codice Iva                                            */
/*       *-------------------------------------------------------*/
  cod_iva     integer (05)  not null COMMENT 'Codice Iva',
/*       *-------------------------------------------------------*/
/*       * Imponibile acquisti                                   */
/*       *                                                       */
/*       * N.B.: se codice iva a zero e mese zero rappresenta il */
/*       *       credito iva relativo all'inizio anno            */
/*       *                                                       */
/*       *       Se codice iva a zero e mese presente rappresen- */
/*       *       ta il credito del mese                          */
/*       *-------------------------------------------------------*/
  ibl_acq     bigint (13)   not null COMMENT 'Imponibile acquisti',
/*       *-------------------------------------------------------*/
/*       * Imposta acquisti                                      */
/*       *-------------------------------------------------------*/
  imp_acq     bigint (13)   not null COMMENT 'Imposta acquisti',
/*       *-------------------------------------------------------*/
/*       * Imponibile vendite                                    */
/*       *                                                       */
/*       * N.B.: se codice iva a zero e mese presente, rappre-   */
/*       *       senta il debito iva del mese e si presume che   */
/*       *       venga versato                                   */
/*       *-------------------------------------------------------*/
  ibl_ven     bigint (13)   not null COMMENT 'Imponibile vendite',
/*       *-------------------------------------------------------*/
/*       * Imposta vendite                                       */
/*       *-------------------------------------------------------*/
  imp_ven     bigint (13)   not null COMMENT 'Imposta vendite',
/*       *-------------------------------------------------------*/
/*       * Imponibile corrispettivi                              */
/*       *                                                       */
/*       * N.B.: se codice iva a zero e mese presente, rappre-   */
/*       *       senta il valore di compensazione con altri      */
/*       *       oneri fiscali, su credito di iva                */
/*       *-------------------------------------------------------*/
  ibl_cor     bigint (13)   not null COMMENT 'Imponibile corrispettivi',
/*       *-------------------------------------------------------*/
/*       * Imposta corrispettivi                                 */
/*       *-------------------------------------------------------*/
  imp_cor     bigint (13)   not null COMMENT 'Imposta corrispettivi'
