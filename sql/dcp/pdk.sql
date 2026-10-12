/*    *==========================================================*/
/*    * Tabella [pdk] Descrizioni alternative prodotti     {dcp} */
/*    *                                                          */
/*    * Aggiornamento 31.05.2024                                 */
/*    *                                                          */
/*    * -------------------------------------------------------- */
/*    *                                                          */
/*    *  N.B.   :  Questo e' un file di chiavi fatto per soppe-  */
/*    *            rire ad un carenza del file 'rf-dcp' e inglo- */
/*    *            ba chiavi anche di 'rf-pdx', 'rf-aaf' e       */
/*    *            'rf-aaq'                                      */
/*    *                                                          */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tip_rec,
                                                        num_pro,
                                                        cod_arc),
/*       *=======================================================*/
/*       * Tipo record                                           */
/*       *                                                       */
/*       * - 'C' : Per Cliente                                   */
/*       * - 'P' : Per Produttore                                */
/*       * - 'F' : Per Fornitore                                 */
/*       * - 'N' : Nomenclatura Combinata                        */
/*       * - 'B' : Barcode alternativo (della confezione)        */
/*       * - 'D' : Barcode alternativo (dell' imballo)           */
/*       *-------------------------------------------------------*/
  tip_rec                   varchar (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice prodotto numerico                              */
/*       *-------------------------------------------------------*/
  num_pro                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice archivio                                       */
/*       *                                                       */
/*       *                                                       */
/*       * Se tipo record 'C' : Codice Cliente                   */
/*       * Se tipo record 'P' : Codice Produttore                */
/*       * Se tipo record 'F' : Codice Fornitore                 */
/*       * Se tipo record 'N' : Codice Fornitore                 */
/*       * Se tipo record 'B' : A zero (Valutare eventuale       */
/*       *                              quantita' confezione)    */
/*       *-------------------------------------------------------*/
  cod_arc                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice prodotto alfanumerico                          */
/*       *-------------------------------------------------------*/
  alf_pro                   varchar (20)                not null
