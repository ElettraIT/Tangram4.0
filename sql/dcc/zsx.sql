/*    *==========================================================*/
/*    * Tabella [zsx] Sconti incrociati                    {dcc} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tip_inc,
                                                        cod_uno,
                                                        cod_due),
/*       *=======================================================*/
/*       * Tipo di incrocio                                      */
/*       *                                                       */
/*       * - 01 : Incrocio tra                                   */
/*       *        - Categorie sconto in riga cliente             */
/*       *        - Categorie sconto prodotto                    */
/*       *                                                       */
/*       * - 02 : Incrocio tra                                   */
/*       *        - Codice cliente                               */
/*       *        - Categorie sconto prodotto                    */
/*       *-------------------------------------------------------*/
  tip_inc                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * 1. codice per l'incrocio                              */
/*       *                                                       */
/*       * - Se tipo incrocio 01 : codice categoria di sconto in */
/*       *                         riga associato al cliente     */
/*       *                                                       */
/*       * - Se tipo incrocio 02 : codice cliente                */
/*       *-------------------------------------------------------*/
  cod_uno                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * 2. codice per l'incrocio                              */
/*       *                                                       */
/*       * - Se tipo incrocio 01 : codice categoria di sconto    */
/*       *                         associato al prodotto         */
/*       *                                                       */
/*       * - Se tipo incrocio 02 : codice categoria di sconto    */
/*       *                         associato al prodotto         */
/*       *-------------------------------------------------------*/
  cod_due                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * % di sconto associate all'incrocio                    */
/*       *-------------------------------------------------------*/
  per_sc1                   decimal (05,02)             not null,
  per_sc2                   decimal (05,02)             not null,
  per_sc3                   decimal (05,02)             not null,
  per_sc4                   decimal (05,02)             not null,
  per_sc5                   decimal (05,02)             not null