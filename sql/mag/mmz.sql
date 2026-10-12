/*    *==========================================================*/
/*    * Tabella [mmz] Saldi quantita' di o presso terzi    {mag} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (ann_ese,
                                                        cod_dpz,
                                                        tip_mag,
                                                        num_mag,
                                                        var_mag,
                                                        tip_ctm,
                                                        cod_ctm,
                                                        tip_arc,
                                                        cod_arc,
                                                        dpz_arc,
                                                        inx_mes),
/*       *=======================================================*/
/*       * Anno di esercizio                                     */
/*       *-------------------------------------------------------*/
  ann_ese                   integer (04)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza                                     */
/*       *-------------------------------------------------------*/
  cod_dpz                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo codice di magazzino                              */
/*       *                                                       */
/*       * - 01 : Prodotti di vendita                            */
/*       * - 02 : Semilavorati                                   */
/*       * - 03 : Materie prime                                  */
/*       * - 04 : Materiali vari                                 */
/*       *-------------------------------------------------------*/
  tip_mag                   integer (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice prodotto numerico                              */
/*       *-------------------------------------------------------*/
  num_mag                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Sigla della variante di magazzino                     */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  var_mag                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Conto merce                                           */
/*       *                                                       */
/*       * - N : Merce presso terzi ma non loro                  */
/*       * - T : Merce presso di noi ma non nostra               */
/*       *-------------------------------------------------------*/
  tip_ctm                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice conto merce                                    */
/*       *-------------------------------------------------------*/
  cod_ctm                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo archivio                                         */
/*       *                                                       */
/*       * - C : Archivio clienti                                */
/*       * - F : Archivio fornitori                              */
/*       * - A : Agente (Incaricato alla vendita)                */
/*       *-------------------------------------------------------*/
  tip_arc                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice archivio                                       */
/*       *                                                       */
/*       * - Se tip-arc C : Codice cliente                       */
/*       * - Se tip-arc F : Codice fornitore                     */
/*       * - Se tip-arc A : Codice agente                        */
/*       *-------------------------------------------------------*/
  cod_arc                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza del codice archivio                 */
/*       *-------------------------------------------------------*/
  dpz_arc                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dislocazione                                   */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  cod_dsl                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Indice mensile                                        */
/*       *                                                       */
/*       * N.B. : mese zero per inizio esercizio                 */
/*       *-------------------------------------------------------*/
  inx_mes                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Progressivo a credito                                 */
/*       *-------------------------------------------------------*/
  prg_cre                   decimal (13,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Progressivo a debito                                  */
/*       *-------------------------------------------------------*/
  prg_deb                   decimal (13,03)             not null
