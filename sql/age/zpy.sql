/*    *==========================================================*/
/*    * Record tabella provvigioni incrociate [zpy]              */
/*    *                                                          */
/*    * Estensione a tabella [zpv]                               */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tip_cpv,
                                                        cod_cpv,
                                                        prg_cpv),
/*       *=======================================================*/
/*       * Tipo categoria di provvigione                         */
/*       * - 01 : Categorie di provvigione legate all'agente     */
/*       * - 02 : Categorie di provvigione legate al prodotto    */
/*       * - 03 : Categorie di provvigione legate al cliente     */
/*       *-------------------------------------------------------*/
  tip_cpv                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice categoria di provvigione                       */
/*       *-------------------------------------------------------*/
  cod_cpv                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Progressivo                                           */
/*       *-------------------------------------------------------*/
  prg_cpv                   integer (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Trattamento provvigioni indirette                     */
/*       *  - 01 : Uguali alle provvigioni dirette               */
/*       *  - 02 : Diverse dalle provvigioni dirette             */
/*       *-------------------------------------------------------*/
  trt_pvi                   integer (02)                not null,
/*       *-------------------------------------------------------*/
/*       * A partire dallo sconto del ...                        */
/*       *-------------------------------------------------------*/
  per_sr1                   decimal (05,02)             not null,
  per_sr2                   decimal (05,02)             not null,
  per_sr3                   decimal (05,02)             not null,
  per_sr4                   decimal (05,02)             not null,
  per_sr5                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * % di sconto unica, interna, calcolata in base alle    */
/*       * cinque percentuali di sconto espresse visivamente     */
/*       *-------------------------------------------------------*/
  per_scu                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * La % di provvigione del ...                           */
/*       *-------------------------------------------------------*/
  per_pv1                   decimal (05,02)             not null,
  per_pv2                   decimal (05,02)             not null,
  per_pv3                   decimal (05,02)             not null