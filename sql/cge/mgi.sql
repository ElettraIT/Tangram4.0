/*    *==========================================================*/
/*    * Tabella [mgi] (ex [moi]) Movimenti IVA                   */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_prt),
/*       *-------------------------------------------------------*/
/*       * Chiavi addizionali                                    */
/*       *-------------------------------------------------------*/
  INDEX                     DATDOC                     (tip_rec,
                                                        dat_reg,
                                                        cod_num,
                                                        prt_iva,
                                                        num_prt),
/*       *=======================================================*/
/*       * Tipo record   : 'C' = clienti                         */
/*       *                 'F' = fornitori                       */
/*       *                 'V' = corrispettivi                   */
/*       *-------------------------------------------------------*/
  tip_rec                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di contabilita'          */
/*       *-------------------------------------------------------*/
  dat_reg                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo movimento di contabilita'           */
/*       *-------------------------------------------------------*/
  num_prt                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Data documento                                        */
/*       *-------------------------------------------------------*/
  dat_doc                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero documento                                      */
/*       *-------------------------------------------------------*/
  num_doc                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice numerazione                                    */
/*       *-------------------------------------------------------*/
  cod_num                   integer (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Protocollo iva per stampa su giornale                 */
/*       *-------------------------------------------------------*/
  prt_iva                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice causale contabile                              */
/*       *-------------------------------------------------------*/
  cod_cau                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo movimento iva                                    */
/*       *                                                       */
/*       * - 1 : Dare cliente Italia o Estero non CEE            */
/*       * - 2 : Avere cliente Italia o Estero non CEE           */
/*       * - 3 : Corrispettivi Italia o Estero non CEE           */
/*       *                                                       */
/*       * - 4 : Dare fornitore Italia o Estero non CEE          */
/*       * - 5 : Avere fornitore Italia o Estero non CEE         */
/*       * - 6 : Bolla doganale Estero non CEE                   */
/*       *                                                       */
/*       * - A : Dare cliente Estero CEE                         */
/*       * - B : Avere cliente Estero CEE                        */
/*       *                                                       */
/*       * - D : Dare fornitore Estero CEE                       */
/*       * - E : Avere fornitore Estero CEE                      */
/*       *-------------------------------------------------------*/
  tip_iva                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Castelletto : elemento 1                              */
/*       *                                                       */
/*       * - Imponibile                                          */
/*       * - Codice IVA                                          */
/*       * - Imposta                                             */
/*       *-------------------------------------------------------*/
  ibl_iv1                   bigint  (13)                not null,
  cod_iv1                   integer (05)                not null,
  imp_iv1                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Castelletto : elemento 2                              */
/*       *-------------------------------------------------------*/
  ibl_iv2                   bigint  (13)                not null,
  cod_iv2                   integer (05)                not null,
  imp_iv2                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Castelletto : elemento 3                              */
/*       *-------------------------------------------------------*/
  ibl_iv3                   bigint  (13)                not null,
  cod_iv3                   integer (05)                not null,
  imp_iv3                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Castelletto : elemento 4                              */
/*       *-------------------------------------------------------*/
  ibl_iv4                   bigint  (13)                not null,
  cod_iv4                   integer (05)                not null,
  imp_iv4                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Castelletto : elemento 5                              */
/*       *-------------------------------------------------------*/
  ibl_iv5                   bigint  (13)                not null,
  cod_iv5                   integer (05)                not null,
  imp_iv5                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Castelletto : elemento 6                              */
/*       *-------------------------------------------------------*/
  ibl_iv6                   bigint  (13)                not null,
  cod_iv6                   integer (05)                not null,
  imp_iv6                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Totale documento                                      */
/*       *-------------------------------------------------------*/
  tot_doc                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice cliente o fornitore                            */
/*       *-------------------------------------------------------*/
  cod_arc                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag di avvenuta stampa giornale                      */
/*       *                                                       */
/*       * Per giornale iva vendite o corrispettivi:             */
/*       * - Spaces : registrazione non stampata                 */
/*       * - S      : registrazione stampata                     */
/*       *                                                       */
/*       * Per giornale iva acquisti:                            */
/*       * - Spaces : registrazione non stampata in assoluto     */
/*       * - S      : stampata solo nel giornale iva acquisti    */
/*       * - I      : stampata solo nel giornale iva a debito su */
/*       *            acquisti intracomunitari                   */
/*       * - T      : stampata sia nel giornale iva acquisti che */
/*       *            nel giornale iva a debito su acquisti in-  */
/*       *            tracomunitari                              */
/*       *-------------------------------------------------------*/
  flg_gio                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag se il movimento si riferisce all'anno corrente o */
/*       * a quello precedente, relativo alle Note di Variazione */
/*       *                                                       */
/*       * - Spaces : relativo all'anno in corso                 */
/*       * - P      : relativo all'anno precedente               */
/*       *                                                       */
/*       * N.B.: flag non piu' utilizzato in quanto l'elenco     */
/*       *       clienti e fornitori non e' piu' obbligatorio    */
/*       *-------------------------------------------------------*/
  flg_cop                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag di regime fiscale particolare                    */
/*       *                                                       */
/*       * - Spaces o N : Normale regime fiscale                 */
/*       * - S          : Regime di Scissione Pagamenti          */
/*       * - R          : Regime di Reverse Charge               */
/*       * - E          : Esclusione da dichiarazione annuale    */
/*       *-------------------------------------------------------*/
  flg_rfp                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice fornitore reale per bolla doganale             */
/*       *-------------------------------------------------------*/
  cod_fbd                   integer (07)                not null

