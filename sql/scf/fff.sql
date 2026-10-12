/*    *==========================================================*/
/*    * Tabella [ccc] Anagrafica controllo fornitori       {scf} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_fnt),
/*       *=======================================================*/
/*       * Codice fornitore                                      */
/*       *                                                       */
/*       * Nota : Le informazioni per il controllo del fornitore */
/*       *        sono valide sia per il fornitore principale    */
/*       *        che per le sue dipendenze                      */
/*       *                                                       */
/*       * Nota : se presente il record zero, vale per tutti i   */
/*       *        fornitori                                      */
/*       *-------------------------------------------------------*/
  cod_fnt                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Esposizione max concessa dal fornitore                */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  max_fid                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di assegnazione                                  */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  dat_fid                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Massimo valore merce per ciascun ordine al fornitore  */
/*       *-------------------------------------------------------*/
  max_orf                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di valutazione                                   */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  dat_orf                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null

