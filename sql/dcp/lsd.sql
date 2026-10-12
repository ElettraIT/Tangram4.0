/*    *==========================================================*/
/*    * Tabella [lsd] Listini storici prodotti             {dcp} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tip_rec,
                                                        cod_lst,
                                                        cod_cli,
                                                        sgl_vlt,
                                                        num_pro,
                                                        dva_fin),
/*       *=======================================================*/
/*       * Tipo record                                           */
/*       *                                                       */
/*       * - 01 : Listini alternativi per prodotti               */
/*       * - 02 : Prezzi netti concordati per cliente            */
/*       * - 03 : Prezzi per campagne promozionali               */
/*       *-------------------------------------------------------*/
  tip_rec                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice listino alternativo                            */
/*       *                                                       */
/*       * Solo se tipo record 01                                */
/*       *-------------------------------------------------------*/
  cod_lst                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice cliente                                        */
/*       *                                                       */
/*       * Solo se tipo record 02                                */
/*       *-------------------------------------------------------*/
  cod_cli                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Sigla valuta per il prezzo di listino                 */
/*       *-------------------------------------------------------*/
  sgl_vlt                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Decimali valuta per il prezzo di listino              */
/*       *-------------------------------------------------------*/
  dec_vlt                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice prodotto numerico                              */
/*       *-------------------------------------------------------*/
  num_pro                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Prezzo                                                */
/*       *                                                       */
/*       * Se tipo record 01 : Prezzo di listino alternativo     */
/*       * Se tipo record 02 : Prezzo netto concordato per il    */
/*       *                     cliente                           */
/*       * Se tipo record 03 : Prezzo per campagna promozionale  */
/*       *-------------------------------------------------------*/
  prz_lst                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Categoria provvigioni associata al listino            */
/*       *-------------------------------------------------------*/
  cat_pvg                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * % provvigioni associate al listino                    */
/*       *-------------------------------------------------------*/
  per_pv1                   decimal (05,02)             not null,
  per_pv2                   decimal (05,02)             not null,
  per_pv3                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Categoria sconto associata al listino                 */
/*       *-------------------------------------------------------*/
  cat_sco                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * % sconto associate al listino                         */
/*       *-------------------------------------------------------*/
  per_sr1                   decimal (05,02)             not null,
  per_sr2                   decimal (05,02)             not null,
  per_sr3                   decimal (05,02)             not null,
  per_sr4                   decimal (05,02)             not null,
  per_sr5                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Si/no utilizzo del prezzo                             */
/*       *-------------------------------------------------------*/
  snx_prz                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/no utilizzo delle % di provvigione nella determi-  */
/*       * nazione delle provvigioni per agente                  */
/*       *-------------------------------------------------------*/
  snx_pvg                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/no utilizzo delle % di sconto nella determinazione */
/*       * degli sconti                                          */
/*       *-------------------------------------------------------*/
  snx_sco                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di validita' iniziale                            */
/*       *                                                       */
/*       * Solo se tipo record 03                                */
/*       *-------------------------------------------------------*/
  dva_ini                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Data di validita' finale                              */
/*       *                                                       */
/*       * Solo se tipo record 03                                */
/*       *-------------------------------------------------------*/
  dva_fin                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Data di validita' finale in formato 'reverse'         */
/*       *                                                       */
/*       * Si ottiene con il seguente calcolo:                   */
/*       * rf-lsd-dvf-rev = (99999999-(19000000+rf-lsd-dva-fin)) */
/*       *                                                       */
/*       * Se rf-lsd-dva-fin vale zero, questo campo viene for-  */
/*       * zato al valore 99999999                               */
/*       *                                                       */
/*       * E' da osservare pertanto che record senza data di va- */
/*       * lidita' finale avrebbero qui il valore 99999999       */
/*       *                                                       */
/*       * In ogni caso un prezzo storicizzato non puo' mancare  */
/*       * mai della data di validita' finale                    */
/*       *-------------------------------------------------------*/
  dvf_rev                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Quantita' di riferimento                              */
/*       *                                                       */
/*       * Per implementare gli scaglioni a quantita' (utiliz-   */
/*       * zare eventualmente il codice cliente come progressivo */
/*       * scaglione)                                            */
/*       *                                                       */
/*       * N.B.: Campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  qta_rif                   decimal (10,03)             not null
