/*    *==========================================================*/
/*    * Tabella [dcd] Clienti commerciali                  {dcc} */
/*    *                                                          */
/*    * Dipendenze                                               */
/*    *                                                          */
/*    * Aggiornamento 10.05.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_cli,
                                                        dpz_cli),
/*       *=======================================================*/
/*       * Codice cliente, deve trovare corrispondenza in        */
/*       * anagrafica clienti per contabilita'                   */
/*       *-------------------------------------------------------*/
  cod_cli                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza cliente                             */
/*       *-------------------------------------------------------*/
  dpz_cli                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Ragione sociale                                       */
/*       *-------------------------------------------------------*/
  rag_soc                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Indirizzo                                             */
/*       *-------------------------------------------------------*/
  via_dcc                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Localita'                                             */
/*       *-------------------------------------------------------*/
  loc_dcc                   varchar (80)                not null,

/*       * ___ DA IMPLEMENTARE ___                            */

/*       *-------------------------------------------------------*/
/*       * Localita' - CAP                                       */
/*       *-------------------------------------------------------*/
/*loc_cap                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Localita' - Comune                                    */
/*       *-------------------------------------------------------*/
/*loc_cmn                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Localita' - Provincia                                 */
/*       *-------------------------------------------------------*/
/*loc_prv                   varchar (10)                not null,




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
/*       * Note ad uso interno                                   */
/*       *                                                       */
/*       * ___ ELIMINARE vedi dcx ___                            */
/*       *-------------------------------------------------------*/
  not_g01                   varchar (80)                not null,
  not_g02                   varchar (80)                not null,
  not_g03                   varchar (80)                not null,



/*       *-------------------------------------------------------*/
/*       * Codice Destinatario per invio dei documenti in        */
/*       * formato elettronico tramite SDI                       */
/*       *                                                       */
/*       * - '999999#PA'   : Destinatario Pubbl. Amminist.       */
/*       * - '9999999#PR'  : Destinatario Privato                */
/*       * - '0000000#PEC' : Destinatario Privato - solo PEC     */
/*       *                                                       */
/*       * N.B. : Campo riutilizzato                             */
/*       *                                                       */
/*       * N.B. : Se codice dipendenza diverso da spaces, il     */
/*       *        valore non e' significativo, in quanto va-     */
/*       *        le quello espresso in anagrafica commer-       */
/*       *        ciale del cliente principale.                  */
/*       *                                                       */
/*       * ___ ELIMINARE vedi epk ___                            */
/*       *-------------------------------------------------------*/
  cod_sdi                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice Destinatario per invio dei documenti in        */
/*       * formato elettronico tramite EDI                       */
/*       *                                                       */
/*       * ___ ELIMINARE vedi epk ___                            */
/*       *-------------------------------------------------------*/
  cod_edi                   varchar (20)                not null,





/*       *-------------------------------------------------------*/
/*       * Codice lingua                                         */
/*       *-------------------------------------------------------*/
  cod_lng                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice ABI dell'istituto di credito di appoggio per   */
/*       * gli effetti                                           */
/*       *-------------------------------------------------------*/
  cod_abi                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice CAB dell'istituto di credito di appoggio per   */
/*       * gli effetti                                           */
/*       *-------------------------------------------------------*/
  cod_cab                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice conto corrente nella banca del cliente, solo   */
/*       * per accrediti pre-autorizzati in conto                */
/*       *-------------------------------------------------------*/
  ccc_app                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice CIN relativo alla combinazione dei precedenti: */
/*       *                                                       */
/*       *  - Codice ABI                                         */
/*       *  - Codice CAB                                         */
/*       *  - Codice conto corrente nella banca                  */
/*       *-------------------------------------------------------*/
  cod_cin                   varchar (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice nostra banca presentazione effetti relativi al */
/*       * cliente                                               */
/*       *-------------------------------------------------------*/
  nos_bpe                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice agente                                         */
/*       *-------------------------------------------------------*/
  cod_age                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice sub-agente                                     */
/*       *-------------------------------------------------------*/
  sub_age                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice categoria provvigionale                        */
/*       *-------------------------------------------------------*/
  cat_pvg                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * % provvigioni                                         */
/*       *-------------------------------------------------------*/
  per_pv1                   decimal (05,02)             not null,
  per_pv2                   decimal (05,02)             not null,
  per_pv3                   decimal (05,02)             not null,
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
/*       * Data di acquisizione del cliente                      */
/*       *-------------------------------------------------------*/
  dat_aqz                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di cliente con accettazione ordini bloccata   */
/*       *                                                       */
/*       * - 01 : No, ordini accettabili                         */
/*       * - 02 : Si, ordini non accettabili                     */
/*       *-------------------------------------------------------*/
  for_blo                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data in cui e' avvenuto il blocco/sblocco della       */
/*       * accettabilita' degli ordini                           */
/*       *-------------------------------------------------------*/
  dor_blo                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di cliente con accettazione ordini bloccata   */
/*       *                                                       */
/*       * - 01 : No, ordini accettabili                         */
/*       * - 02 : Si, ordini non accettabili                     */
/*       *-------------------------------------------------------*/
  fco_blo                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data in cui e' avvenuto il blocco/sblocco delle       */
/*       * consegne                                              */
/*       *-------------------------------------------------------*/
  dco_blo                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Trasporto a cura abituale                             */
/*       *                                                       */
/*       *  - 01 : Nessuna preferenza                            */
/*       *                                                       */
/*       *  - 10 : Mittente                                      */
/*       *  - 11 : Mittente + Destinatario                       */
/*       *  - 12 : Mittente + Vettore                            */
/*       *  - 13 : Mittente + Vettore + Destinatario             */
/*       *  - 14 : Mittente + Vettore + Vettore                  */
/*       *                                                       */
/*       *  - 20 : Destinatario                                  */
/*       *                                                       */
/*       *  - 30 : Vettore                                       */
/*       *  - 31 : Vettore + Destinatario                        */
/*       *  - 32 : Vettore + Vettore                             */
/*       *  - 33 : Vettore + Vettore + Destinatario              */
/*       *  - 34 : Vettore + Vettore + Vettore                   */
/*       *-------------------------------------------------------*/
  dtt_acu                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice vettore abituale                               */
/*       *-------------------------------------------------------*/
  cod_vet                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * 2. vettore abituale                                   */
/*       *-------------------------------------------------------*/
  cod_vt2                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * 3. vettore abituale                                   */
/*       *-------------------------------------------------------*/
  cod_vt3                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero di abbonamento con il vettore                  */
/*       *-------------------------------------------------------*/
  abn_vtt                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Distanza in chilometri dalla ns. sede, o dalla ns.    */
/*       * dipendenza che normalmente serve il cliente           */
/*       *-------------------------------------------------------*/
  dst_klm                   decimal (06,01)             not null,
/*       *-------------------------------------------------------*/
/*       * Chiusura per ferie, data inizio                       */
/*       *-------------------------------------------------------*/
  dti_cpf                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Chiusura per ferie, data fine                         */
/*       *-------------------------------------------------------*/
  dtf_cpf                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Settimana lavorativa, ovvero giorni di apertura       */
/*       *                                                       */
/*       * Ogni singolo carattere di questo campo indica un      */
/*       * giorno della settimana, il primo carattere sta' per   */
/*       * il Lunedi', ed il settimo per la Domenica             */
/*       *                                                       */
/*       * Ogni carattere puo' assumere il seguente valore:      */
/*       *                                                       */
/*       *  - A : Aperto tutto il giorno                         */
/*       *  - C : Chiuso tutto il giorno                         */
/*       *  - M : Aperto solamente al mattino                    */
/*       *  - P : Aperto solamente al pomeriggio                 */
/*       *                                                       */
/*       * Se questo campo e' tutto a spaces, esso sara' in-     */
/*       * terpretato come se fosse a 'AAAAACC', ovvero come     */
/*       * chiusura Sabato e Domenica                            */
/*       *-------------------------------------------------------*/
  gdl_nls                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Orario di apertura mattino, inizio                    */
/*       *-------------------------------------------------------*/
  oam_oin                   integer (04)                not null,
/*       *-------------------------------------------------------*/
/*       * Orario di apertura mattino, fine                      */
/*       *-------------------------------------------------------*/
  oam_ofi                   integer (04)                not null,
/*       *-------------------------------------------------------*/
/*       * Orario di apertura pomeriggio, inizio                 */
/*       *-------------------------------------------------------*/
  oap_oin                   integer (04)                not null,
/*       *-------------------------------------------------------*/
/*       * Orario di apertura pomeriggio, fine                   */
/*       *-------------------------------------------------------*/
  oap_ofi                   integer (04)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
