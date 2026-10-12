/*    *==========================================================*/
/*    * Tabella [zfx] Forme di pagamento clienti           {dcc} */
/*    *                                                          */
/*    * Descrizione in lingua                                    */
/*    *                                                          */
/*    * Aggiornamento 10.04.2026                                 */
/*    *                                                          */
/*    * ___ VALUTARE SOPPRESSIONE ___                            */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tip_rec,
                                                        cod_fop,
                                                        cod_lng),
/*       *=======================================================*/
/*       * Tipo record                                           */
/*       *                                                       */
/*       * - 01 : Estensione al file [zfp] per descrizioni in    */
/*       *        lingua italiana alternative, fino a 400 carat- */
/*       *        teri                                           */
/*       *                                                       */
/*       * - 02 : Estensione al file [zfp] per descrizioni in    */
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
/*       * Descrizione alternativa oppure in lingua              */
/*       *-------------------------------------------------------*/
  des_alt                   varchar (80)                not null