/*    *==========================================================*/
/*    * Tabella [dfd] Fornitori commerciali                {dcf} */
/*    *                                                          */
/*    * Dipendenze                                               */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_fnt,
                                                        dpz_fnt),
/*       *=======================================================*/
/*       * Codice fornitore, deve trovare corrispondenza in      */
/*       * anagrafica fornitori per contabilita'                 */
/*       *-------------------------------------------------------*/
  cod_fnt                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza fornitore                           */
/*       *-------------------------------------------------------*/
  dpz_fnt                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Ragione sociale in uppercase                          */
/*       *-------------------------------------------------------*/
  rag_key                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Ragione sociale                                       */
/*       *-------------------------------------------------------*/
  rag_soc                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Indirizzo                                             */
/*       *-------------------------------------------------------*/
  via_dcf                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Localita'                                             */
/*       *-------------------------------------------------------*/
  loc_dcf                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice nazione                                        */
/*       *-------------------------------------------------------*/
  cod_naz                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice comune                                         */
/*       *-------------------------------------------------------*/
  cod_cmn                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice frazione                                       */
/*       *-------------------------------------------------------*/
  cod_fzn                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice localita'                                      */
/*       *-------------------------------------------------------*/
  cod_lct                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Ragione sociale per invio documento - parte 1         */
/*       *-------------------------------------------------------*/
  rs1_doc                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Ragione sociale per invio documento - parte 2         */
/*       *-------------------------------------------------------*/
  rs2_doc                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice lingua                                         */
/*       *-------------------------------------------------------*/
  cod_lng                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice ABI dell'istituto di credito di appoggio per   */
/*       * pagamenti da effettuarsi al fornitore a mezzo banca,  */
/*       * per effettuare bonifici bancari al fornitore          */
/*       *-------------------------------------------------------*/
  cod_abi                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice CAB dell'istituto di credito di appoggio per   */
/*       * pagamenti da effettuarsi al fornitore a mezzo banca,  */
/*       * per effettuare bonifici bancari al fornitore          */
/*       *-------------------------------------------------------*/
  cod_cab                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice conto corrente nella banca del fornitore di    */
/*       * appoggio per pagamenti da effettuarsi al fornitore a  */
/*       * mezzo banca, per effettuare bonifici bancari al       */
/*       * fornitore                                             */
/*       *-------------------------------------------------------*/
  ccc_app                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice conto corrente postale di appoggio per pa-     */
/*       * gamenti da effettuarsi al fornitore a mezzo c/c       */
/*       * postale, per effettuare bonifici postali al fornitore */
/*       *-------------------------------------------------------*/
  ccp_app                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di acquisizione del fornitore                    */
/*       *-------------------------------------------------------*/
  dat_aqz                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Codice zona                                           */
/*       *-------------------------------------------------------*/
  cod_zon                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice categoria                                      */
/*       *-------------------------------------------------------*/
  cod_cat                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice statistico                                     */
/*       *-------------------------------------------------------*/
  cod_stt                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di fornitore con blocco sugli ordini          */
/*       *                                                       */
/*       * - 01 : No, si possono fare ordini al fornitore        */
/*       * - 02 : Si, ordini al fornitore bloccati               */
/*       *-------------------------------------------------------*/
  for_blo                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data in cui e' avvenuto il blocco/sblocco della       */
/*       * eseguibilita' degli ordini                            */
/*       *-------------------------------------------------------*/
  dor_blo                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Codice CIN italiano                                   */
/*       *-------------------------------------------------------*/
  cin_ita                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice CIN europeo                                    */
/*       *-------------------------------------------------------*/
  cin_eur                   varchar (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
