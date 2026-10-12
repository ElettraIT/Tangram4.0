/*    *==========================================================*/
/*    * Tabella [zts] Stagionalita' scorte                 {fab} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *                                                          */
/*    * ___ TUTTO DA RIVEDERE ___                                */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tip_stg),
/*       *=======================================================*/
/*       * Tipo stagionalita'                                    */
/*       *-------------------------------------------------------*/
  tip_stg                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione tipo stagionalita'                        */
/*       *-------------------------------------------------------*/
  des_stg                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione tipo stagionalita' in uppercase           */
/*       *-------------------------------------------------------*/
  des_key                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Commento al tipo stagionalita'                        */
/*       *-------------------------------------------------------*/
  com_stg                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero parametri di stagionalita' : 1..4              */
/*       *-------------------------------------------------------*/
  num_pst                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Parametro di stagionalita' 1                          */
/*       *                                                       */
/*       * - Descrizione parametro                               */
/*       * - Limite di inizio per il parametro di stagionalita'  */
/*       *  (formato mm/gg)                                      */
/*       * - Limite di fine per il parametro di stagionalita'    */
/*       *  (formato mm/gg)                                      */
/*       *-------------------------------------------------------*/
  des_ps1                   varchar (10)                not null,
  ini_ps1                   integer (04)                not null,
  fin_ps1                   integer (04)                not null,
/*       *-------------------------------------------------------*/
/*       * Parametro di stagionalita' 2                          */
/*       *                                                       */
/*       * - Descrizione parametro                               */
/*       * - Limite di inizio per il parametro di stagionalita'  */
/*       *  (formato mm/gg)                                      */
/*       * - Limite di fine per il parametro di stagionalita'    */
/*       *  (formato mm/gg)                                      */
/*       *-------------------------------------------------------*/
  des_ps2                   varchar (10)                not null,
  ini_ps2                   integer (04)                not null,
  fin_ps2                   integer (04)                not null,
/*       *-------------------------------------------------------*/
/*       * Parametro di stagionalita' 3                          */
/*       *                                                       */
/*       * - Descrizione parametro                               */
/*       * - Limite di inizio per il parametro di stagionalita'  */
/*       *  (formato mm/gg)                                      */
/*       * - Limite di fine per il parametro di stagionalita'    */
/*       *  (formato mm/gg)                                      */
/*       *-------------------------------------------------------*/
  des_ps3                   varchar (10)                not null,
  ini_ps3                   integer (04)                not null,
  fin_ps3                   integer (04)                not null,
/*       *-------------------------------------------------------*/
/*       * Parametro di stagionalita' 4                          */
/*       *                                                       */
/*       * - Descrizione parametro                               */
/*       * - Limite di inizio per il parametro di stagionalita'  */
/*       *  (formato mm/gg)                                      */
/*       * - Limite di fine per il parametro di stagionalita'    */
/*       *  (formato mm/gg)                                      */
/*       *-------------------------------------------------------*/
  des_ps4                   varchar (10)                not null,
  ini_ps4                   integer (04)                not null,
  fin_ps4                   integer (04)                not null