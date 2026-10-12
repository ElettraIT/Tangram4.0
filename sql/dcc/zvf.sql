/*    *==========================================================*/
/*    * Tabella [zvf] Voci descrittive clienti             {dcc} */
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
/*       *                                                       */
/*       *  - da 101 a 199 : prompts per stampa fattura          */
/*       *                                                       */
/*       *  - da 301 a 399 : prompts per stampa bolla            */
/*       *                                                       */
/*       *  - da 501 a 599 : prompts per stampa ordine spediz.   */
/*       *                                                       */
/*       *  - da 901 a 999 : prompts per stampa conferma ordine  */
/*       *                                                       */
/*       *  ---------------------------------------------------  */
/*       *                                                       */
/*       *  - 100          : prompts per tipo fattura            */
/*       *  - 300          : prompts per tipo bolla              */
/*       *  - 500          : prompts per tipo ordine spedizione  */
/*       *  - 900          : prompts per tipo ordine             */
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
/*       *                                                       */
/*       * Se a space, definizione della voce descrittiva gene-  */
/*       * rica                                                  */
/*       *-------------------------------------------------------*/
  cod_lng                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione per la stampa                             */
/*       *-------------------------------------------------------*/
  des_stp                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice spesa eventualmente associato                  */
/*       *                                                       */
/*       * Solo per le voci descrittive libere (001..006)        */
/*       *-------------------------------------------------------*/
  num_spf                   integer (03)                not null
