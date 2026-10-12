/*    *==========================================================*/
/*    * Tabella [yvf] Voci descrittive fornitori           {dcf} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_def,
                                                        cod_def,
                                                        cod_lng),
/*       *=======================================================*/
/*       * Numero voce descrittiva per fatturazione 1..6         */
/*       *                                                       */
/*       *  - da 001 a 006 : voci descrittive libere             */
/*       *  - da 901 a 999 : prompts per stampa conferma ordine  */
/*       *-------------------------------------------------------*/
  num_def                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice voce descrittiva per fatturazione              */
/*       *                                                       */
/*       * Solo per le voci descrittive libere (001..006)        */
/*       *                                                       */
/*       * Se numero voce 100, 300, 500 o 900 questo campo       */
/*       * contiene il codice tipo movimento                     */
/*       *-------------------------------------------------------*/
  cod_def                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice lingua                                         */
/*       *-------------------------------------------------------*/
  cod_lng                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione per la stampa                             */
/*       *-------------------------------------------------------*/
  des_stp                   varchar (40)                not null
