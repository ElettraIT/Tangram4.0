/*    *==========================================================*/
/*    * Tabella [ffr] Fatture fornitori : righe            {ffo} */
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
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  cod_tmo                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice nostra dipendenza che ha ricevuto il documento */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  cod_dpz                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione                                    */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  dat_reg                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Tipo archivio per il movimento                        */
/*       *                                                       */
/*       * - F : Fornitore                                       */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  tip_arc                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice archivio                                       */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  cod_arc                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza                                     */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  dpz_arc                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Data documento del fornitore                          */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  dat_doc                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero documento del fornitore                        */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  num_doc                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice lingua per il documento                        */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  cod_lng                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Valuta per la fatturazione                            */
/*       *                                                       */
/*       * - Sigla                                               */
/*       * - Decimali                                            */
/*       * - Tipo cambio                                         */
/*       * - Cambio                                              */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  sgl_vpf                   varchar (03)                not null,
  dec_vpf                   tinyint (01)                not null,
  tdc_vpf                   varchar (01)                not null,
  cdc_vpf                   decimal (12,05)             not null,
/*       *-------------------------------------------------------*/
/*       * Tipo riga                                             */
/*       *                                                       */
/*       * P   : Prodotti di vendita                             */
/*       * M   : Materie prime                                   */
/*       * V   : Materiali vari                                  */
/*       * PN  : Prodotti non codificati                         */
/*       *                                                       */
/*       * A   : Addebito o Accredito a forfait                  */
/*       *                                                       */
/*       * C   : Commento                                        */
/*       *-------------------------------------------------------*/
  tip_rig                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo codice di magazzino                              */
/*       *                                                       */
/*       * - 00 : Non significativo                              */
/*       * - 01 : Prodotti di vendita                            */ 
/*       *       (se tipo riga P o PN)                           */
/*       * - 03 : Materie prime                                  */
/*       *       (se tipo riga M)                                */
/*       * - 04 : Materiale vario                                */
/*       *       (se tipo riga V)                                */
/*       *-------------------------------------------------------*/
  tip_mag                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice numerico di magazzino                          */
/*       *                                                       */
/*       * N.B. : Solo per i tipi riga                           */
/*       *        - P  Codice numerico su [dcp]                  */
/*       *        - M  Codice numerico su [dpm]                  */
/*       *        - V  Codice numerico su [mtv]                  */
/*       *        - PN Codice numerico su [dcp]                  */
/*       *             del similare                              */
/*       *-------------------------------------------------------*/
  num_mag                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice alfanumerico di magazzino                      */
/*       *                                                       */
/*       * N.B. : Solo per i tipi riga                           */
/*       *        - P  Codice alfanumerico su [dcp]              */
/*       *        - M  Codice alfanumerico su [dpm]              */
/*       *        - V  Codice alfanumerico su [mtv]              */
/*       *        - PN Codice alfanumerico su [dcp]              */
/*       *             del similare                              */
/*       *-------------------------------------------------------*/
  alf_mag                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Sigla variante per il codice magazzino                */
/*       *                                                       */
/*       * N.B.: Campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  sgl_vrn                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Formato di acquisizione                               */
/*       *-------------------------------------------------------*/
  fda_pif                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice prodotto per il fornitore                      */
/*       *-------------------------------------------------------*/
  cop_sfn                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No trasformazione unita' di misura tra codifica    */
/*       * del fornitore e codifica nostra                       */
/*       *                                                       */
/*       *   - N : Nessuna trasformazione                        */
/*       *                                                       */
/*       *   - S : Si trasformazione, il fornitore utilizza per  */
/*       *         la vendita una diversa unita' di misura ri-   */
/*       *         spetto a quella utilizzata nella gestione del */
/*       *         magazzino, pertanto quantita' e prezzo validi */
/*       *         per l'ordine si riferiscono all'unita' di mi- */
/*       *         sura adottata dal fornitore                   */
/*       *                                                       */
/*       *   - P : Si trasformazione, il fornitore utilizza per  */
/*       *         la vendita una diversa unita' di misura ri-   */
/*       *         spetto a quella utilizzata nella gestione del */
/*       *         magazzino, pero' accetta ordini nella unita'  */
/*       *         di misura utilizzata nella gestione del ma-   */
/*       *         gazzino, fermo restando pero' che fatturera'  */
/*       *         in termini della sua unita' di misura         */
/*       *-------------------------------------------------------*/
  snx_tum            varchar (01)         not null,
/*       *-------------------------------------------------------*/
/*       * Unita' di misura utilizzata dal fornitore             */
/*       *                                                       */
/*       *   - Se la voce snx-tum e' pari a 'N' questa voce vie- */
/*       *     ne posta a Spaces, perche' sempre uguale alla u-  */
/*       *     nita' di misura valida per la gestione magazzino  */
/*       *                                                       */
/*       *   - Se la voce snx-tum e' pari a 'S' questa voce vie- */
/*       *     ne posta al valore dell'unita' di misura valida   */
/*       *     per l'emissione ordine al fornitore, sia in ter-  */
/*       *     mini di quantita' che di prezzo                   */
/*       *                                                       */
/*       *   - Se la voce snx-tum e' pari a 'P' questa voce vie- */
/*       *     ne posta al valore dell'unita' di misura valida   */
/*       *     per l'emissione ordine al fornitore esclusiva-    */
/*       *     mente in termini di prezzo                        */
/*       *-------------------------------------------------------*/
  umf_tum                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero decimali utilizzato dal fornitore per esprime- */
/*       * re la quantita' nella sua unita' di misura            */
/*       *                                                       */
/*       *   - Se la voce snx-tum e' pari a 'N' questa voce vie- */
/*       *     ne posta a Zero, perche' sempre uguale al numero  */
/*       *     decimali valido per la gestione magazzino         */
/*       *                                                       */
/*       *   - Se la voce snx-tum e' pari a 'S' questa voce vie- */
/*       *     ne posta al valore del numero decimali valido     */
/*       *     per l'emissione ordine al fornitore, sia in ter-  */
/*       *     mini di quantita' che di prezzo                   */
/*       *                                                       */
/*       *   - Se la voce snx-tum e' pari a 'P' questa voce vie- */
/*       *     ne posta al valore del numero decimali valido     */
/*       *     per l'emissione ordine al fornitore esclusiva-    */
/*       *     mente in termini di prezzo                        */
/*       *-------------------------------------------------------*/
  nde_tum                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Coefficiente moltiplicatore, per cui va' moltiplicata */
/*       * la quantita' espressa nell'unita' di misura del for-  */
/*       * nitore, per ridurla alla unita' di misura utilizzata  */
/*       * da noi                                                */
/*       *                                                       */
/*       * Solo se snx-tum pari a 'S' o 'P', altrimenti zero     */
/*       *-------------------------------------------------------*/
  cmo_tum                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Coefficiente divisore, per cui va' divisa la quanti-  */
/*       * ta' espressa nell'unita' di misura del fornitore, per */
/*       * ridurla alla unita' di misura utilizzata da noi       */
/*       *                                                       */
/*       * Solo se snx-tum pari a 'S' o 'P', altrimenti zero     */
/*       *-------------------------------------------------------*/
  cdi_tum                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Presenza della descrizione estesa di max 400 caratte- */
/*       * ri in file di estensione                              */
/*       *                                                       */
/*       *                                                       */
/*       * - 0 : No                                              */
/*       *                                                       */
/*       *       Cio' avviene nei seguenti casi                  */
/*       *       - Il tipo riga e' P,M,V e la descrizione, pari  */
/*       *         alla descrizione originale nell' anagrafica   */
/*       *         [aaf] o [dcp] o [dpm] o [mtv], oppure variata */
/*       *         rispetto ad essa, non supera i 40 caratteri   */
/*       *       - Il tipo riga e' PN,A,C, e la descrizione di-  */
/*       *         gitata non supera i 40 caratteri              */
/*       *                                                       */
/*       *                                                       */
/*       * - 1 : Si, in file di estensione descrizione per bol-  */
/*       *           le fornitori [bfx]                          */
/*       *                                                       */
/*       *       Cio' avviene nei seguenti casi                  */
/*       *       - Il tipo riga e' P,M,V e la descrizione, di-   */
/*       *         versa dalla descrizione originale in anagra-  */
/*       *         fica [aaf] o [dcp] o [dpm] o [mtv], supera i  */
/*       *         40 caratteri                                  */
/*       *       - Il tipo riga e' PN,A,C, e la descrizione di-  */
/*       *         gitata supera i 40 caratteri                  */
/*       *                                                       */
/*       *                                                       */
/*       * - 2 : Si, in file di estensione descrizione per pro-  */
/*       *           dotti [pdx], tipo record 01                 */
/*       *       - Il tipo riga e' P, e la descrizione, uguale   */
/*       *         alla descrizione originale in anagrafica      */
/*       *         [dcp], supera i 40 caratteri                  */
/*       *                                                       */
/*       * - 3 : Si, in file di estensione descrizione per pro-  */
/*       *           dotti [pdx], tipo record 13                 */
/*       *       - Il tipo riga e' P, e la descrizione, uguale   */
/*       *         alla descrizione originale in anagrafica      */
/*       *         [aaf], supera i 40 caratteri                  */
/*       *                                                       */
/*       * N.B. : Se il valore e' pari a 1, la descrizione este- */
/*       *        sa sostituisce completamente la descrizione di */
/*       *        40 caratteri contenuta in questo record.       */
/*       *-------------------------------------------------------*/
  des_ext                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione per la riga bolla fornitore               */
/*       *                                                       */
/*       * - Se tipo riga P,M,V                                  */
/*       *                                                       */
/*       *   - Se des-ext 0 : Descrizione originale o modifica-  */
/*       *                    ta,  che comunque non supera glo-  */
/*       *                    balmente i 40 caratteri            */
/*       *                                                       */
/*       *   - Se des-ext 1 : Descrizione originale dell'anagra- */
/*       *                    fica [dcp] o [dpm] o [mtv] di 40   */
/*       *                    caratteri                          */
/*       *                                                       */
/*       *   - Se des-ext 2 : Descrizione originale dell'anagra- */
/*       *                    fica [dcp] di 40 caratteri         */
/*       *                                                       */
/*       *                                                       */
/*       * - Se tipo riga PN,A,C                                 */
/*       *                                                       */
/*       *   - Se des-ext 0 : Descrizione impostata, la quale    */
/*       *                    non supera globalmente i 40 ca-    */
/*       *                    ratteri                            */
/*       *                                                       */
/*       *   - Se des-ext 1 : Primi 40 caratteri della descri-   */
/*       *                    zione impostata, la quale supera   */
/*       *                    globalmente i 40 caratteri         */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  des_rig                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo prodotto                                         */
/*       *                                                       */
/*       * - Se tipo riga P o PN                                 */
/*       *                                                       */
/*       *   - 01 : Merce                                        */
/*       *   - 02 : Servizio                                     */
/*       *   - 03 : Imballo                                      */
/*       *   - 09 : Extra attivita' aziendale                    */
/*       *                                                       */
/*       * - Se tipo riga M                                      */
/*       *                                                       */
/*       *   - 01 : Merce                                        */
/*       *                                                       */
/*       * - Se tipo riga V                                      */
/*       *                                                       */
/*       *   - 01 : Merce                                        */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  tip_pro                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice iva per l'acquisto                             */
/*       *-------------------------------------------------------*/
  cod_iva                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice contropartita per l'acquisto                   */
/*       *-------------------------------------------------------*/
  ctp_acq                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Unita' di misura per l'acquisto                       */
/*       *                                                       */
/*       * N.B. : Solo per i tipi riga                           */
/*       *        - P  Da anagrafica [dcp]                       */
/*       *        - M  Da anagrafica [dpm]                       */
/*       *        - V  Da anagrafica [mtv]                       */
/*       *        - PN Da anagrafica [dcp] del similare          */
/*       *                                                       */
/*       * Sempre rispetto alla gestione del magazzino           */
/*       *-------------------------------------------------------*/
  umi_acq                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero decimali quantita' per l'acquisto : 0..3       */
/*       *                                                       */
/*       * N.B. : Solo per i tipi riga                           */
/*       *        - P  Da anagrafica [dcp]                       */
/*       *        - M  Da anagrafica [dpm]                       */
/*       *        - V  Da anagrafica [mtv]                       */
/*       *        - PN Da anagrafica [dcp] del similare          */
/*       *                                                       */
/*       * Sempre rispetto alla gestione del magazzino           */
/*       *-------------------------------------------------------*/
  dec_qta                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Quantita' acquistata secondo il formato di acquisi-   */
/*       * zione del fornitore                                   */
/*       *                                                       */
/*       *   - Se la voce snx-tum e' pari a 'N' questa voce cor- */
/*       *     risponde alla quantita' acquistata, valida sia    */
/*       *     per la gestione magazzino, sia per il controllo   */
/*       *     dell'evasione dell'ordine, sia per l'applicazione */
/*       *     del prezzo unitario di acquisto                   */
/*       *   - Se la voce snx-tum e' pari a 'S' questa voce cor- */
/*       *     risponde alla quantita' acquistata, valida sia    */
/*       *     per il controllo dell'evasione dell'ordine, sia   */
/*       *     per l'applicazione del prezzo unitario di acqui-  */
/*       *     sto, ma non per la gestione del magazzino         */
/*       *   - Se la voce snx-tum e' pari a 'P' questa voce cor- */
/*       *     risponde alla quantita' acquistata, valida solo   */
/*       *     perl'applicazione del prezzo unitario di acquisto,*/
/*       *     ma non per il controllo dell'evasione dell'ordi-  */
/*       *     ne, ne' per la gestione del magazzino             */
/*       *                                                       */
/*       * N.B. : Solo per i tipi riga P,M,V,PN                  */
/*       *-------------------------------------------------------*/
  qta_fda                   decimal (13,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Quantita' acquistata                                  */
/*       *                                                       */
/*       *   - Se la voce snx-tum e' pari a 'N' questa voce vie- */
/*       *     ne posta pari alla quantita' acquistata secondo   */
/*       *     il formato di acquisizione, valida sia per la ge- */
/*       *     stione magazzino, sia per il controllo dell'eva-  */
/*       *     sione dell'ordine, sia per l'applicazione del     */
/*       *     prezzo unitario di acquisto                       */
/*       *   - Se la voce snx-tum e' pari a 'S' questa voce vie- */
/*       *     ne posta pari alla quantita' acquistata valida    */
/*       *     per la gestione del magazzino, ma non per l'eva-  */
/*       *     sione dell'ordine, ne' per l'applicazione del     */
/*       *     prezzo unitario d'acquisto                        */
/*       *   - Se la voce snx-tum e' pari a 'P' questa voce vie- */
/*       *     ne posta pari alla quantita' acquistata valida    */
/*       *     per il controllo dell'evasione dell'ordine e per  */
/*       *     la gestione del magazzino, ma non per l'applica-  */
/*       *     zione del prezzo unitario di acquisto             */
/*       *                                                       */
/*       * N.B. : Solo per i tipi riga P,M,V,PN                  */
/*       *-------------------------------------------------------*/
  qta_acq                   decimal (13,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Codice dislocazione effettivo, da tipo movimento      */
/*       *-------------------------------------------------------*/
  cod_dsl                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di gestione della seconda quantita' nella     */
/*       * fatturazione                                          */
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
/*       * fatturazione                                          */
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
/*       * Numero decimali per il prezzo d'acquisto              */
/*       *-------------------------------------------------------*/
  dec_prz                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Valuta per l'espressione del prezzo di acquisto       */
/*       *                                                       */
/*       * N.B.: Sempre pari alla valuta di fatturazione         */
/*       *                                                       */
/*       * - Sigla                                               */
/*       * - Decimali                                            */
/*       * - Tipo cambio                                         */
/*       * - Cambio                                              */
/*       *-------------------------------------------------------*/
  sgl_vpp                   varchar (03)                not null,
  dec_vpp                   tinyint (01)                not null,
  tdc_vpp                   varchar (01)                not null,
  cdc_vpp                   decimal (12,05)             not null,
/*       *-------------------------------------------------------*/
/*       * Prezzo unitario di acquisto, in valuta                */
/*       *                                                       */
/*       * Attenzione : Il prezzo e' sempre espresso come numero */
/*       *              intero, ma deve essere considerato con   */
/*       *              tanti decimali quanti specificati alla   */
/*       *              voce precedente, inoltre bisogna tenere  */
/*       *              conto degli ulteriori decimali espressi  */
/*       *              dai decimali valuta.                     */
/*       *-------------------------------------------------------*/
  prz_acq                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di gestione del secondo prezzo nella fattura- */
/*       * zione                                                 */
/*       * - 0 : No                                              */
/*       * - 1 : Si                                              */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  snx_2pz                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero decimali del secondo prezzo                    */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  dec_2pz                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Secondo prezzo, in valuta                             */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  prz_a02                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Valuta per l'espressione del legame valutario         */
/*       *                                                       */
/*       * - Sigla                                               */
/*       * - Decimali                                            */
/*       * - Tipo cambio                                         */
/*       * - Prezzo di riferimento, in valuta                    */
/*       * - Cambio                                              */
/*       * - Cambio di riferimento                               */
/*       * - Percentuale di limitazione                          */
/*       * - Tipo di limitazione                                 */
/*       *   - T : Tolleranza                                    */
/*       *   - F : Franchigia                                    */
/*       *   - M : Massimo tetto                                 */
/*       * - Momento di applicazione                             */
/*       *   - F : Al momento della fattura                      */
/*       *-------------------------------------------------------*/
  sgl_vpl                   varchar (03)                not null,
  dec_vpl                   tinyint (01)                not null,
  tdc_vpl                   varchar (01)                not null,
  prz_vpl                   bigint  (11)                not null,
  cdc_vpl                   decimal (12,05)             not null,
  ccr_vpl                   decimal (12,05)             not null,
  plm_vpl                   decimal (05,02)             not null,
  tlm_vpl                   varchar (01)                not null,
  map_vpl                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di esposizione della riga di fattura             */
/*       *                                                       */
/*       * - Se tipo riga P o Pn                                 */
/*       *                                                       */
/*       *    - 1 : Quantita', prezzo, sconti, importo           */
/*       *    - 2 : Solo quantita' e importo                     */
/*       *    - 3 : Solo importo                                 */
/*       *    - 4 : Solo importo se quantita pari a 1,000, al-   */
/*       *               trimenti tutto come il tipo 1           */
/*       *    - 5 : Solo importo se quantita pari a 1,000, al-   */
/*       *               trimenti tutto come il tipo 2           */
/*       *                                                       */
/*       * - Se tipo riga S o M                                  */
/*       *                                                       */
/*       *    - 1 : Quantita', prezzo, sconti, importo           */
/*       *                                                       */
/*       * - Se tipo riga V                                      */
/*       *                                                       */
/*       *    - 1 : Quantita', prezzo, sconti, importo           */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  epz_rgf                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Categoria di sconto in riga associata al prodotto     */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  csr_aap                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Precentuali di sconto in riga associate al prodotto   */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  psr_ap1                   decimal (05,02)             not null,
  psr_ap2                   decimal (05,02)             not null,
  psr_ap3                   decimal (05,02)             not null,
  psr_ap4                   decimal (05,02)             not null,
  psr_ap5                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Percentuali di sconto in riga 1..5                    */
/*       *-------------------------------------------------------*/
  per_sr1                   decimal (05,02)             not null,
  per_sr2                   decimal (05,02)             not null,
  per_sr3                   decimal (05,02)             not null,
  per_sr4                   decimal (05,02)             not null,
  per_sr5                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Prezzo netto effettivo, in valuta                     */
/*       *-------------------------------------------------------*/
  prz_net                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo definitivo per la riga, in valuta             */
/*       *-------------------------------------------------------*/
  imp_rig                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo ausiliario per gestione blocchi, in valuta    */
/*       *-------------------------------------------------------*/
  iau_rig                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo documento bolla del fornitore                    */
/*       *-------------------------------------------------------*/
  bfo_tip                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione bolla del fornitore                */
/*       *-------------------------------------------------------*/
  bfo_dat                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Protocollo documento bolla del fornitore              */
/*       *-------------------------------------------------------*/
  bfo_prt                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Progressivo riga documento bolla del fornitore        */
/*       *-------------------------------------------------------*/
  bfo_prg                   bigint  (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Data documento bolla del fornitore                    */
/*       *-------------------------------------------------------*/
  bfo_ddo                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero documento bolla del fornitore                  */
/*       *-------------------------------------------------------*/
  bfo_ndo                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Flags di elaborazione bloccanti                       */
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
/*       * Flags di elaborazione NON bloccanti                   */
/*       *                                                       */
/*       * 01 : (libero)                                         */
/*       * 02 : (libero)                                         */
/*       * 03 : (libero)                                         */
/*       *-------------------------------------------------------*/
  flg_nb1                   varchar (01)                not null,
  flg_nb2                   varchar (01)                not null,
  flg_nb3                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag di sottoponibilita a pulizia                     */
/*       *-------------------------------------------------------*/
  flg_pul                   varchar (01)                not null

