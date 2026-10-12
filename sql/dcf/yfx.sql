/*    *==========================================================*/
/*    * Tabella [yfx] Forme di pagamento da fornitori      {dcf} */
/*    *                                                          */
/*    * Descrizione in lingua                                    */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tip_rec,
                                                        cod_fop,
                                                        cod_lng,
                                                        num_prg),
/*       *=======================================================*/
/*       * Tipo record                                           */
/*       *                                                       */
/*       * - 01 : Estensione al file [yfp] per descrizioni in    */
/*       *        lingua italiana alternative, fino a 400 carat- */
/*       *        teri                                           */
/*       *                                                       */
/*       * - 02 : Estensione al file [yfp] per descrizioni in    */
/*       *        lingua estera, fino a 400 caratteri            */
/*       *                                                       */
/*       * N.B. : Attualmente limitato a 40 caratteri            */
/*       *-------------------------------------------------------*/
  tip_rec                   integer (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice forma di pagamento                             */
/*       *-------------------------------------------------------*/
  cod_fop                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice lingua                                         */
/*       *-------------------------------------------------------*/
  cod_lng                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero progressivo                                    */
/*       *-------------------------------------------------------*/
  num_prg                   integer (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione alternativa oppure in lingua              */
/*       *                                                       */
/*       * N.B. : Questa descrizione e' solo la decima parte     */
/*       *        dell'intera descrizione, la quale risulta      */
/*       *        composta da un minimo di uno ad un massimo     */
/*       *        di dieci parti, numerate rispettivamente con   */
/*       *        i numeri progressivi da 01 a 10, compattate    */
/*       *        a fisarmonica partendo dal numero progressi-   */
/*       *        vo 01.                                         */
/*       *-------------------------------------------------------*/
  des_alt                   varchar (40)                not null