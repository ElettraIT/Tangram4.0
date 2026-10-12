/*    *==========================================================*/
/*    * Tabella [zca] Causali di ammortamento                    */
/*    *                                                          */
/*    * Aggiornamento 24.09.2025                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_cau),
/*       *=======================================================*/
/*       * Codice causale                                        */
/*       *-------------------------------------------------------*/
  cod_cau                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione                                           */
/*       *-------------------------------------------------------*/
  des_cau                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Mnemonico                                             */
/*       *-------------------------------------------------------*/
  cod_mne                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Segno movimento                                       */
/*       *                                                       */
/*       * - decremento (ammortamento, vendita)                  */
/*       * + incremento (acquisto)                               */
/*       *-------------------------------------------------------*/
  sgn_mov                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * % di ammortamento di default                          */
/*       *                                                       */
/*       * N.B.: campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  per_amr                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * % di valore                                           */
/*       *                                                       */
/*       * N.B.: campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  per_val                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Tipo archivio                                         */
/*       *                                                       */
/*       *  - 'N' = Nessuno                                      */
/*       *  - 'F' = Fornitori                                    */
/*       *  - 'C' = Clienti                                      */
/*       *-------------------------------------------------------*/
  tip_arc                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flags di trattamento causale                          */
/*       *                                                       */
/*       * 01 : Si/no acquisto del bene                          */
/*       * 02 : Si/no trattamento valore iniziale                */
/*       * 03 : Si/no trattamento % ammortamento                 */
/*       * 04 : Si/no trattamento rif. documento                 */
/*       * 05 : Si/no trattamento matricola agg.                 */
/*       * 06 : Si/no cessione totale del bene                   */
/*       *                                                       */
/*       * 07 : (libero)                                         */
/*       * 08 : (libero)                                         */
/*       * 09 : (libero)                                         */
/*       * 10 : (libero)                                         */
/*       *-------------------------------------------------------*/
  flg_t01                   varchar (01)                not null,
  flg_t02                   varchar (01)                not null,
  flg_t03                   varchar (01)                not null,
  flg_t04                   varchar (01)                not null,
  flg_t05                   varchar (01)                not null,
  flg_t06                   varchar (01)                not null,
  flg_t07                   varchar (01)                not null,
  flg_t08                   varchar (01)                not null,
  flg_t09                   varchar (01)                not null,
  flg_t10                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
