/*    *==========================================================*/
/*    * Record tabella provvigioni incrociate [zpx]              */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tip_inc,
                                                        cod_uno,
                                                        cod_due,
                                                        prg_cpv),
/*       *=======================================================*/
/*       * Tipo di incrocio                                      */
/*       *                                                       */
/*       * - 01 : Incrocio tra                                   */
/*       *        - Categorie provvigione agenti                 */
/*       *        - Categorie provvigione prodotti               */
/*       *                                                       */
/*       * - 02 : Incrocio tra                                   */
/*       *        - Categorie provvigione agenti                 */
/*       *        - Categorie provvigione clienti                */
/*       *                                                       */
/*       * - 03 : Incrocio tra                                   */
/*       *        - Categorie provvigione clienti                */
/*       *        - Categorie provvigione prodotti               */
/*       *-------------------------------------------------------*/
  tip_inc                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * 1. codice per l'incrocio                              */
/*       * - Se tipo incrocio 01 : cod. cat provv. agente        */
/*       * - Se tipo incrocio 02 : cod. cat provv. agente        */
/*       * - Se tipo incrocio 03 : cod. cat provv. cliente       */
/*       *-------------------------------------------------------*/
  cod_uno                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * 2. codice per l'incrocio                              */
/*       * - Se tipo incrocio 01 : cod. cat provv. prodotto      */
/*       * - Se tipo incrocio 02 : cod. cat provv. cliente       */
/*       * - Se tipo incrocio 03 : cod. cat provv. prodotto      */
/*       *-------------------------------------------------------*/
  cod_due                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Progressivo                                           */
/*       *-------------------------------------------------------*/
  prg_cpv                   integer (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Trattamento provvigioni indirette                     */
/*       *                                                       */
/*       *  - 01 : Uguali alle provvigioni dirette               */
/*       *  - 02 : Diverse dalle provvigioni dirette             */
/*       *-------------------------------------------------------*/
  trt_pvi                   integer (02)                not null,
/*       *-------------------------------------------------------*/
/*       * % di sconto per lo scaglione                          */
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
/*       * % di provvigione                                      */
/*       *-------------------------------------------------------*/
  per_pv1                   decimal (05,02)             not null,
  per_pv2                   decimal (05,02)             not null,
  per_pv3                   decimal (05,02)             not null