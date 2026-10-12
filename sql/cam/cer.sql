/*    *==========================================================*/
/*    * Tabella [cer] Cespiti ammortizzabili : righe             */
/*    *                                                          */
/*    * Aggiornamento 08.09.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_csp,
                                                        num_prg),
/*       *=======================================================*/
/*       * Codice cespite                                        */
/*       *-------------------------------------------------------*/
  cod_csp                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero progressivo                                    */
/*       *-------------------------------------------------------*/
  num_prg                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice causale                                        */
/*       *-------------------------------------------------------*/
  cod_cau                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Data documento di riferimento                         */
/*       *-------------------------------------------------------*/
  dat_doc                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero documento di riferimento                       */
/*       *-------------------------------------------------------*/
  num_doc                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Protocollo IVA documento di riferimento               */
/*       *-------------------------------------------------------*/
  prt_doc                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo documento di riferimento                      */
/*       *-------------------------------------------------------*/
  imp_doc                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione di contabilita'                    */
/*       *-------------------------------------------------------*/
  dtr_cge                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Protocollo registrazione di contabilita'              */
/*       *-------------------------------------------------------*/
  prt_cge                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Segno movimento                                       */
/*       *                                                       */
/*       * - decremento (ammortamento, vendita)                  */
/*       * + incremento (acquisto)                               */
/*       *-------------------------------------------------------*/
  sgn_mov                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo archivio                                         */
/*       *                                                       */
/*       *  - N = Nessuno                                        */
/*       *  - F = Fornitori                                      */
/*       *  - C = Clienti                                        */
/*       *-------------------------------------------------------*/
  tip_arc                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice archivio                                       */
/*       *-------------------------------------------------------*/
  cod_arc                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza archivio                            */
/*       *                                                       */
/*       * N.B.: campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  dpz_arc                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Data movimento                                        */
/*       *-------------------------------------------------------*/
  dat_mov                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Importo movimento                                     */
/*       *-------------------------------------------------------*/
  imp_mov                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Importi ausiliari                                     */
/*       *                                                       */
/*       * N.B.: campi attualmente non gestiti                   */
/*       *-------------------------------------------------------*/
  imp_au1                   bigint  (13)                not null,
  imp_au2                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * % di ammortamento applicata                           */
/*       *-------------------------------------------------------*/
  per_amr                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * % di ammortamento ausiliarie                          */
/*       *                                                       */
/*       * N.B.: campi attualmente non gestiti                   */
/*       *-------------------------------------------------------*/
  per_au1                   bigint  (13)                not null,
  per_au2                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Matricola accessorio                                  */
/*       *-------------------------------------------------------*/
  mtr_acc                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Commento in riga                                      */
/*       *-------------------------------------------------------*/
  com_rig                   text                        not null,
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
  flg_t10                   varchar (01)                not null
