/*    *==========================================================*/
/*    * Tabella [ocp] Ordini clienti                       {orc} */
/*    *                                                          */
/*    * Estensione a righe per assegnazioni                      */
/*    *                                                          */
/*    * Aggiornamento 14.09.2025                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_prt,
                                                        num_prg,
                                                        prg_frm),
/*       *=======================================================*/
/*       * Numero protocollo : s.aa.dd.nnnnnn                    */
/*       *-------------------------------------------------------*/
  num_prt                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Progressivo riga                                      */
/*       *-------------------------------------------------------*/
  num_prg                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero progressivo frammento riga ordine cliente      */
/*       *=======================================================*/
  prg_frm                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Quantita' assegnata al frammento                      */
/*       *-------------------------------------------------------*/
  qta_ass                   decimal (13,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Data assegnata al frammento                           */
/*       *                                                       */
/*       * N.B.: Valore a zero per quantita' assegnata alla gia- */
/*       *       cenza                                           */
/*       *-------------------------------------------------------*/
  dat_ass                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo ordine fornitore                    */
/*       *                                                       */
/*       * N.B.: Solo se assegnazione relativa ad un ordine for- */
/*       *       nitore                                          */
/*       *-------------------------------------------------------*/
  prt_orf                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero progressivo nel protocollo ordine fornitore    */
/*       *                                                       */
/*       * N.B.: Solo se assegnazione relativa ad un ordine for- */
/*       *       nitore                                          */
/*       *-------------------------------------------------------*/
  prg_orf                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Flags di pulizia effettuabile                         */
/*       *-------------------------------------------------------*/
  flg_pul                   varchar (01)                not null
