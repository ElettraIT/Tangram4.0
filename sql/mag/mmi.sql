/*    *==========================================================*/
/*    * Tabella [mmi] Inventario - tabella di appoggio     {mag} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *                                                          */
/*    * ___ SOSTITUITO DA 'mim' ___                              */
/*    *----------------------------------------------------------*/








/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_dpz,
                                                        tip_mag,
                                                        num_mag,
                                                        var_mag,
                                                        tip_gia,
                                                        tip_arc,
                                                        cod_arc,
                                                        dpz_arc,
                                                        cod_ctm),
/*       *=======================================================*/
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
/*       * Tipo giacenza                                         */
/*       *                                                       */
/*       * - 01 : Merci in giacenza, ma al netto delle merci non */
/*       *        di proprieta'                                  */
/*       * - 02 : Merci presso di noi, ma non di ns. proprieta'  */
/*       * - 03 : Merci presso terzi, ma non di loro proprieta'  */
/*       *-------------------------------------------------------*/
  tip_gia                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Sigla conto merce                                     */
/*       *                                                       */
/*       * Nota : Significativo solo per tipo giacenza 02 o 03   */
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
/*       * Codice alfanumerico magazzino                         *
/*       *-------------------------------------------------------*/
  alf_mag                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione magazzino                                 */
/*       *-------------------------------------------------------*/
  des_mag                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Unita' di misura per gestione magazzino               */
/*       *-------------------------------------------------------*/
  umi_mag                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero decimali quantita' per gestione magazzino      */
/*       *-------------------------------------------------------*/
  dec_qta                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo ubicazione                                       */
/*       *-------------------------------------------------------*/
  tip_ubi                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Parametri di ubicazione: 1..6                         */
/*       *-------------------------------------------------------*/
  prm_ub1                   varchar (10)                not null,
  prm_ub2                   varchar (10)                not null,
  prm_ub3                   varchar (10)                not null,
  prm_ub4                   varchar (10)                not null,
  prm_ub5                   varchar (10)                not null,
  prm_ub6                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Quantita' presunta, ovvero quella che dovrebbe essere */
/*       * rilevata in assenza di differenze inventariali, rife- */
/*       * rita alla data di inizio inventario                   */
/*       *-------------------------------------------------------*/
  qta_prs                   decimal (13,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Flag di rilevazione eseguita                          */
/*       *                                                       */
/*       * - Spaces : Non ancora eseguita                        */
/*       * - #      : Eseguita                                   */
/*       *-------------------------------------------------------*/
  flg_ril                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Quantita' rilevata                                    */
/*       *-------------------------------------------------------*/
  qta_rlv                   decimal (13,03)             not null