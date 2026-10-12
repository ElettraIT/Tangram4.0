/*    *==========================================================*/
/*    * Tabella [ydf] Descrizioni per fatturazione         {dcf} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_def,
                                                        cod_lng),
/*       *=======================================================*/
/*       * Numero voce descrittiva per fatturazione 1..6         */
/*       *-------------------------------------------------------*/
  num_def                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice lingua                                         */
/*       *-------------------------------------------------------*/
  cod_lng                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione per video estesa, solo se lingua italiano */
/*       *-------------------------------------------------------*/
  des_ves                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione per video ridotta, solo se italiano       */
/*       *-------------------------------------------------------*/
  des_vri                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione per stampa                                */
/*       *                                                       */
/*       * N.B. : se il codice lingua non e' l'italiano, il re-  */
/*       *        cord comporta solamente questo campo           */
/*       *-------------------------------------------------------*/
  des_stp                   varchar (40)                not null
