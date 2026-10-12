/*    *==========================================================*/
/*    * Tabella [zls] Listini prodotto di vendita          {dcp} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_lst),
/*       *=======================================================*/
/*       * Codice listino                                        */
/*       *-------------------------------------------------------*/
  cod_lst                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione listino                                   */
/*       *-------------------------------------------------------*/
  des_lst                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Sigla valuta per il listino                           */
/*       *-------------------------------------------------------*/
  sgl_vlt                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di listino 'di lavoro'                        */
/*       *                                                       */
/*       *  - 'N' : No, listino normale                          */
/*       *  - 'S' : Si, listino di lavoro                        */
/*       *-------------------------------------------------------*/
  snx_ldl                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Direttive generali listino                            */
/*       *                                                       */
/*       *  - Spaces : listino normale, senza direttive          */
/*       *                                                       */
/*       *  - I      : listino comprensivo di aliquota fiscale   */
/*       *                                                       */
/*       *  - K      : listino con prezzi relativi a 1000 unita' */
/*       *  - C      : listino con prezzi relativi a 100 unita'  */
/*       *  - D      : listino con prezzi relativi a 10 unita'   */
/*       *                                                       */
/*       *  - M      : listino maggiorato in %                   */
/*       *  - S      : listino scontato in %                     */
/*       *                                                       */
/*       *  N.B.: Attualmente sono implementate solo le opzioni  */
/*       *        'K', 'M' e 'S'                                 */
/*       *-------------------------------------------------------*/
  drg_lst                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice listino di riferimento                         */
/*       *                                                       */
/*       *  N.B.: solo se direttive listino 'M' o 'S'            */
/*       *-------------------------------------------------------*/
  rif_lst                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * % di sconto o maggiorazione                           */
/*       *                                                       */
/*       *  N.B.: solo se direttive listino 'M' o 'S'            */
/*       *-------------------------------------------------------*/
  per_sm1                   decimal (05,02)             not null,
  per_sm2                   decimal (05,02)             not null,
  per_sm3                   decimal (05,02)             not null,
  per_sm4                   decimal (05,02)             not null,
  per_sm5                   decimal (05,02)             not null
