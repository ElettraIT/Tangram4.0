/*    *==========================================================*/
/*    * Tabella [vpr] Versamenti da produzione : righe     {vdp} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_prt,
                                                        num_prg),
/*       *=======================================================*/
/*       * Numero protocollo : s.aa.dd.nnnnnn                    */
/*       *-------------------------------------------------------*/
  num_prt                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero progressivo riga                               */
/*       *-------------------------------------------------------*/
  num_prg                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo movimento                                        */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  tmo_vdp                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo codice di magazzino                              */
/*       *                                                       */
/*       * - 03 : Materia prima                                  */
/*       *-------------------------------------------------------*/
  tip_mag                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice numerico di magazzino                          */
/*       *-------------------------------------------------------*/
  num_mag                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice alfanumerico di magazzino                      */
/*       *-------------------------------------------------------*/
  alf_mag                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Sigla della variante per il codice magazzino          */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  sgl_vrn                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Unita' di misura per la produzione, da anagrafica     */
/*       *-------------------------------------------------------*/
  umi_prd                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero decimali quantita' per la produzione : 0..3,   */
/*       * da anagrafica                                         */
/*       *-------------------------------------------------------*/
  dec_qta                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Quantita' consegnata                                  */
/*       *-------------------------------------------------------*/
  qta_cns                   decimal (13,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Codice dislocazione effettivo                         */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  cod_dsl                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di gestione della seconda quantita' nella     */
/*       * produzione                                            */
/*       * - 0 : No                                              */
/*       * - 1 : Si                                              */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  snx_2qt                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero decimali per la seconda quantita'              */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  dec_2qt                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Seconda quantita'                                     */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  qta_a02                   decimal (13,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di gestione della terza quantita' nella       */
/*       * produzione                                            */
/*       * - 0 : No                                              */
/*       * - 1 : Si                                              */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  snx_3qt                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero decimali per la terza quantita'                */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  dec_3qt                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Terza quantita'                                       */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  qta_a03                   decimal (13,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Flags bloccanti                                       */
/*       *                                                       */
/*       * 01 : (libero)                                         */
/*       * 02 : (libero)                                         */
/*       * 03 : (libero)                                         */
/*       * 04 : (libero)                                         */
/*       * 05 : (libero)                                         */
/*       * 06 : (libero)                                         */
/*       * 07 : (libero)                                         */
/*       *-------------------------------------------------------*/
  flg_bl1                   varchar (01)                not null,
  flg_bl2                   varchar (01)                not null,
  flg_bl3                   varchar (01)                not null,
  flg_bl4                   varchar (01)                not null,
  flg_bl5                   varchar (01)                not null,
  flg_bl6                   varchar (01)                not null,
  flg_bl7                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flags non bloccanti                                   */
/*       *                                                       */
/*       * 01 : (libero)                                         */
/*       * 02 : (libero)                                         */
/*       * 03 : (libero)                                         */
/*       *-------------------------------------------------------*/
  flg_nb1                   varchar (01)                not null,
  flg_nb2                   varchar (01)                not null,
  flg_nb3                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flags di pulizia effettuabile                         */
/*       *-------------------------------------------------------*/
  flg_pul                   varchar (01)                not null
