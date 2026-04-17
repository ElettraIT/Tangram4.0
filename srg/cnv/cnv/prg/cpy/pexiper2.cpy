      *    *===========================================================*
      *    * Work per salvataggio valori provenienti da segreteria     *
      *    *-----------------------------------------------------------*
       01  w-sgr.
      *        *-------------------------------------------------------*
      *        * Tabella di decodifica fasi                            *
      *        *-------------------------------------------------------*
           05  w-sgr-nam-rdf.
               10  w-sgr-nam-max          pic  9(03)       value 197  .
               10  w-sgr-nam-lun          pic  9(02)       value 48   .
               10  w-sgr-nam-ctr          pic  9(03)                  .
               10  w-sgr-nam-tbl.
                   15  filler             pic  x(48) value
                    "azi adc Utenze                                  ".
                   15  filler             pic  x(48) value
                    "dcf aaf Condizioni di acquisto fornitore/prod.  ".
                   15  filler             pic  x(48) value
                    "dcf aaq Condizioni generali di acquisto prodotto".
                   15  filler             pic  x(48) value
                    "azi ada Azienda - dati anagrafici               ".
                   15  filler             pic  x(48) value
                    "age age Anagrafica agenti                       ".
                   15  filler             pic  x(48) value
                    "age ags Legami tra agenti e super-agenti        ".
                   15  filler             pic  x(48) value
                    "cge ali Riepilogo movimenti per allegati Iva    ".
                   15  filler             pic  x(48) value
                    "abi axc Circuiti interbancari                   ".
                   15  filler             pic  x(48) value
                    "abi axi Istituti di credito                     ".
                   15  filler             pic  x(48) value
                    "abi axs Sportelli degli istituti di credito     ".
                   15  filler             pic  x(48) value
                    "scf bef Anagrafica banche estere                ".
                   15  filler             pic  x(48) value
                    "bfo bfr Bolle fornitori, righe                  ".
                   15  filler             pic  x(48) value
                    "bfo bft Bolle fornitori, testate                ".
                   15  filler             pic  x(48) value
                    "bfo bfx Bolle fornitori, estensioni a testate   ".
                   15  filler             pic  x(48) value
                    "bol bie Bolle clienti, evasioni                 ".
                   15  filler             pic  x(48) value
                    "bol bir Bolle clienti, righe                    ".
                   15  filler             pic  x(48) value
                    "bol bit Bolle clienti, testate                  ".
                   15  filler             pic  x(48) value
                    "bol bix Bolle clienti, estensioni alle testate  ".
                   15  filler             pic  x(48) value
                    "gep cbp Anagrafica casse, banche, c/c postali   ".
                   15  filler             pic  x(48) value
                    "gep ccc Anagrafica controllo crediti clienti    ".
                   15  filler             pic  x(48) value
                    "cge cdb Anagrafica classificatori di bilancio   ".
                   15  filler             pic  x(48) value
                    "cge cea Cespiti                                 ".
                   15  filler             pic  x(48) value
                    "cge cer Cespiti - righe                         ".
                   15  filler             pic  x(48) value
                    "gep cec Controllo esposizione clienti           ".
                   15  filler             pic  x(48) value
                    "cdp cdp Commesse di produzione, testata e riga  ".
                   15  filler             pic  x(48) value
                    "cge cli Anagrafica clienti contabile            ".
                   15  filler             pic  x(48) value
                    "dcc dcc Anagrafica clienti commerciale          ".
                   15  filler             pic  x(48) value
                    "dcf dcf Anagrafica fornitori commerciale        ".
                   15  filler             pic  x(48) value
                    "dcc dcm Anagrafica clienti per il marketing     ".
                   15  filler             pic  x(48) value
                    "dcp dcp Anagrafica prodotti                     ".
                   15  filler             pic  x(48) value
                    "dcc dcx Annotazioni clienti commerciale         ".
                   15  filler             pic  x(48) value
                    "gep ddp Distinte di presentazione effetti       ".
                   15  filler             pic  x(48) value
                    "dcf dfx Annotazioni fornitori commerciale       ".
                   15  filler             pic  x(48) value
                    "dpm dpm Anagrafica materie prime                ".
                   15  filler             pic  x(48) value
                    "dps dps Anagrafica semilavorati                 ".
                   15  filler             pic  x(48) value
                    "fab fbs Parametri per gestione sottoscorta      ".
                   15  filler             pic  x(48) value
                    "ffo ffr Fatture fornitori, righe                ".
                   15  filler             pic  x(48) value
                    "ffo fft Fatture fornitori, testate              ".
                   15  filler             pic  x(48) value
                    "ffo ffx Fatture fornitori, estensioni a testate ".
                   15  filler             pic  x(48) value
                    "fat fir Fatture a clienti, righe                ".
                   15  filler             pic  x(48) value
                    "fat fit Fatture a clienti, testate              ".
                   15  filler             pic  x(48) value
                    "fat fix Fatture a clienti, estensioni a testate ".
                   15  filler             pic  x(48) value
                    "cge fnt Anagrafica fornitori contabile          ".
                   15  filler             pic  x(48) value
                    "gep gep Personalizzazioni S.I.A. per portafoglio".
                   15  filler             pic  x(48) value
                    "age gpc Conteggi provvigionali                  ".
                   15  filler             pic  x(48) value
                    "age gpm Maturazioni su conteggi provvigionali   ".
                   15  filler             pic  x(48) value
                    "azi gva Anagrafica veicoli aziendali            ".
                   15  filler             pic  x(48) value
                    "azi gvs Scadenze relative ai veicoli aziendali  ".
                   15  filler             pic  x(48) value
                    "geo gxc Anagrafica comuni, frazioni, e localita'".
                   15  filler             pic  x(48) value
                    "geo gxn Anagrafica nazioni                      ".
                   15  filler             pic  x(48) value
                    "geo gxp Anagrafica provincie                    ".
                   15  filler             pic  x(48) value
                    "geo gxr Anagrafica regioni                      ".
                   15  filler             pic  x(48) value
                    "geo gxs Anagrafica stradari                     ".
                   15  filler             pic  x(48) value
                    "iic iir Movimenti per Iva intrac., righe        ".
                   15  filler             pic  x(48) value
                    "iic iit Movimenti per Iva intrac., testate      ".
                   15  filler             pic  x(48) value
                    "cge ivp Riepilogo movimenti dichiarazione Iva   ".
                   15  filler             pic  x(48) value
                    "dcf lfd Prezzi acquisto fornitori storicizzati  ".
                   15  filler             pic  x(48) value
                    "dtp lgr Distinte base, righe distinta           ".
                   15  filler             pic  x(48) value
                    "dtp lgt Distinte base, testate distinta         ".
                   15  filler             pic  x(48) value
                    "dtp lgv Anagrafica sub-distinte base virtuali   ".
                   15  filler             pic  x(48) value
                    "cge lic Lettere di intento dei clienti          ".
                   15  filler             pic  x(48) value
                    "dcp lsd Prezzi di listino storicizzati per data ".
                   15  filler             pic  x(48) value
                    "dcp lst Prezzi di listino, e netti concordati   ".
                   15  filler             pic  x(48) value
                    "mag mau Ubicazioni prodotti a magazzino         ".
                   15  filler             pic  x(48) value
                    "mag mim Archivio di appoggio rilevazioni invent.".
                   15  filler             pic  x(48) value
                    "cge mgr Primanota, righe                        ".
                   15  filler             pic  x(48) value
                    "cge mgs Progressivi contabili per sottoconti    ".
                   15  filler             pic  x(48) value
                    "cge mgt Primanota, testate                      ".
                   15  filler             pic  x(48) value
                    "mag mmr Movimenti gestione magazzino, righe     ".
                   15  filler             pic  x(48) value
                    "mag mms Saldi quantita' per gestione magazzino  ".
                   15  filler             pic  x(48) value
                    "mag mmt Movimenti gestione magazzino, testate   ".
                   15  filler             pic  x(48) value
                    "mag mmv Saldi valore per gestione magazzino     ".
                   15  filler             pic  x(48) value
                    "mag mmz Saldi quantita' in conto merce          ".
                   15  filler             pic  x(48) value
                    "cge mgi Primanota, estensione registrazioni Iva ".
                   15  filler             pic  x(48) value
                    "cge mpu Primanota, Modello di Pagamento Unific. ".
                   15  filler             pic  x(48) value
                    "mtv mtv Anagrafica materiali vari               ".
                   15  filler             pic  x(48) value
                    "xpg num Numerazioni varie                       ".
                   15  filler             pic  x(48) value
                    "gep obp Estensioni alle scadenze in portafoglio ".
                   15  filler             pic  x(48) value
                    "orc ocp Assegnazioni a righe ordini clienti     ".
                   15  filler             pic  x(48) value
                    "orc ocr Ordini clienti, righe                   ".
                   15  filler             pic  x(48) value
                    "orc oct Ordini clienti, testate                 ".
                   15  filler             pic  x(48) value
                    "orc ocx Ordini clienti, estensioni alle testate ".
                   15  filler             pic  x(48) value
                    "orf ofr Ordini fornitori, righe                 ".
                   15  filler             pic  x(48) value
                    "orf oft Ordini fornitori, testate               ".
                   15  filler             pic  x(48) value
                    "orf ofx Ordini fornitori, estensioni a testate  ".
                   15  filler             pic  x(48) value
                    "ods osk Ordini di spedizione, righe Packing List".
                   15  filler             pic  x(48) value
                    "ods osr Ordini di spedizione clienti, righe     ".
                   15  filler             pic  x(48) value
                    "ods ost Ordini di spedizione clienti, testate   ".
                   15  filler             pic  x(48) value
                    "ods osx Ordini di spedizione clienti, estensioni".
                   15  filler             pic  x(48) value
                    "cge pdc Anagrafica sottoconti piano dei conti   ".
                   15  filler             pic  x(48) value
                    "dcf pdt Anagrafica case produttrici             ".
                   15  filler             pic  x(48) value
                    "dcp pdx Anagrafica estensioni descrizioni prod. ".
                   15  filler             pic  x(48) value
                    "xpg prs Personalizzazioni generali              ".
                   15  filler             pic  x(48) value
                    "rda raa Archivio anagrafico gestione ritenuta   ".
                   15  filler             pic  x(48) value
                    "rda ram Movimenti gestione ritenuta d'acconto   ".
                   15  filler             pic  x(48) value
                    "cge rdb Movimenti di rettifica                  ".
                   15  filler             pic  x(48) value
                    "xpg ref Referenze generali                      ".
                   15  filler             pic  x(48) value
                    "gep rsc Relazioni tra scadenze debitori         ".
                   15  filler             pic  x(48) value
                    "gep rsd Riscossioni scadenze debitori           ".
                   15  filler             pic  x(48) value
                    "xpg sce Scelte varie, [obsoleto]                ".
                   15  filler             pic  x(48) value
                    "gep scr Solleciti a clienti, righe              ".
                   15  filler             pic  x(48) value
                    "gep sct Solleciti a clienti, testate            ".
                   15  filler             pic  x(48) value
                    "gep sdb Scadenze in portafoglio                 ".
                   15  filler             pic  x(48) value
                    "scf sfa Addebiti su ordini di pagamento         ".
                   15  filler             pic  x(48) value
                    "scf sff Fatture fornitori (da scadenziario)     ".
                   15  filler             pic  x(48) value
                    "scf sfp Pagamenti registrati                    ".
                   15  filler             pic  x(48) value
                    "scf sfs Scadenze fornitori                      ".
                   15  filler             pic  x(48) value
                    "svf svs Set statistiche di vendita sul fatturato".
                   15  filler             pic  x(48) value
                    "xpg tbl Tabelle varie, in via di obsolescenza   ".
                   15  filler             pic  x(48) value
                    "xpg upr User Personal Reminder - promemoria     ".
                   15  filler             pic  x(48) value
                    "bil vbr Voci di bilancio, righe                 ".
                   15  filler             pic  x(48) value
                    "bil vbt Voci di bilancio, testate               ".
                   15  filler             pic  x(48) value
                    "bil vbv Voci di bilancio, valori                ".
                   15  filler             pic  x(48) value
                    "bol vet Anagrafica vettori                      ".
                   15  filler             pic  x(48) value
                    "dcc vlt Valori dei cambi per le valute estere   ".
                   15  filler             pic  x(48) value
                    "vdp vpr Versamenti da produzione, righe consumi ".
                   15  filler             pic  x(48) value
                    "vdp vpt Versamenti da produzione, testata e riga".
                   15  filler             pic  x(48) value
                    "bfo ybf Tabella tipi movimento per bolle fornit.".
                   15  filler             pic  x(48) value
                    "dcf ybo Tabella addebito spese bollo da fornit. ".
                   15  filler             pic  x(48) value
                    "orf yca Tabella descrizioni condizioni acquisto ".
                   15  filler             pic  x(48) value
                    "cdp ycp Tabella tipi movimento per commesse pr. ".
                   15  filler             pic  x(48) value
                    "dcf ydf Tabella definizione voci descr. fornit. ".
                   15  filler             pic  x(48) value
                    "ffo yff Tabella tipi movimento fatture fornitori".
                   15  filler             pic  x(48) value
                    "dcf yfp Tabella forme di pagamento da fornitori ".
                   15  filler             pic  x(48) value
                    "dcf yin Tabella addebito spese incasso fornitori".
                   15  filler             pic  x(48) value
                    "orf yof Tabella tipi movimento ordini fornitori ".
                   15  filler             pic  x(48) value
                    "scf yop Tabella tipi operazione scadenze fornit.".
                   15  filler             pic  x(48) value
                    "cdp yrc Tabella responsabili per commesse prod. ".
                   15  filler             pic  x(48) value
                    "orf yro Tabella responsabili per ordini fornit. ".
                   15  filler             pic  x(48) value
                    "dcf ysf Tabella definizione voci spese fatt. for".
                   15  filler             pic  x(48) value
                    "dcf yst Tabella valori delle voci statistiche   ".
                   15  filler             pic  x(48) value
                    "dcf yvf Tabella valori voci descrittive fatt. f.".
                   15  filler             pic  x(48) value
                    "vdp yvp Tabella tipi movimento versamenti prod. ".
                   15  filler             pic  x(48) value
                    "bol zab Tabella descrizioni aspetto est. beni   ".
                   15  filler             pic  x(48) value
                    "fat zac Tabella descrizioni addebiti o commenti ".
                   15  filler             pic  x(48) value
                    "bol zba Tabella descriz. annotazioni pie' bolla ".
                   15  filler             pic  x(48) value
                    "bol zbi Tabella tipi movimento per bolle clienti".
                   15  filler             pic  x(48) value
                    "dcc zbo Tabella addebito spese bollo in fattura ".
                   15  filler             pic  x(48) value
                    "cge zcc Tabella causali contabili primanota     ".
                   15  filler             pic  x(48) value
                    "cge zci Tabella codici Iva                      ".
                   15  filler             pic  x(48) value
                    "cge zc1 Tabella cespiti                         ".
                   15  filler             pic  x(48) value
                    "cge zca Tabella causali cespiti                 ".
                   15  filler             pic  x(48) value
                    "rda zco Tabella tipi contributi gestione riten. ".
                   15  filler             pic  x(48) value
                    "rda zcr Tabella causali gestione ritenuta d'acc.".
                   15  filler             pic  x(48) value
                    "dcc zcs Tabella categorie di sconto             ".
                   15  filler             pic  x(48) value
                    "bol zct Tabella causali del trasporto           ".
                   15  filler             pic  x(48) value
                    "orc zcv Tabella descrizioni condizioni ordini cl".
                   15  filler             pic  x(48) value
                    "dcc zdf Tabella definizione voci descrittive    ".
                   15  filler             pic  x(48) value
                    "fat zfi Tabella tipi movimento per fatturazione ".
                   15  filler             pic  x(48) value
                    "dcc zfp Tabella forme di pagamento per clienti  ".
                   15  filler             pic  x(48) value
                    "dcc zin Tabella addebito spese incasso fattura  ".
                   15  filler             pic  x(48) value
                    "dcc zkc Tabella codici marketing per clienti    ".
                   15  filler             pic  x(48) value
                    "dcc zln Tabella per la definizione codici lingua".
                   15  filler             pic  x(48) value
                    "dcp zls Tabella tipi di listino per fatturazione".
                   15  filler             pic  x(48) value
                    "dpm zm1 Tabella classi merceologiche materie pr.".
                   15  filler             pic  x(48) value
                    "dpm zm2 Tabella gruppi merceologici materie pr. ".
                   15  filler             pic  x(48) value
                    "dpm zm3 Tabella sottogruppi merceologici mat. pr".
                   15  filler             pic  x(48) value
                    "mag zmc Tabella causali gestione magazzino      ".
                   15  filler             pic  x(48) value
                    "mag zmd Tabella dislocazioni per dipendenza     ".
                   15  filler             pic  x(48) value
                    "mag zmm Tabella tipi conto merce                ".
                   15  filler             pic  x(48) value
                    "dpm zms Tabella classificatori stat. materie pr.".
                   15  filler             pic  x(48) value
                    "mag zmu Tabella tipi ubicazione                 ".
                   15  filler             pic  x(48) value
                    "orc zoc Tabella tipi movimento ordini clienti   ".
                   15  filler             pic  x(48) value
                    "gep zop Tabella tipi operazione gestione portaf.".
                   15  filler             pic  x(48) value
                    "azi zos Tabella filtri di ordinamento selezione ".
                   15  filler             pic  x(48) value
                    "dcp zp1 Tabella classi merceologiche prodotti   ".
                   15  filler             pic  x(48) value
                    "dcp zp2 Tabella gruppi merceologici per prodotti".
                   15  filler             pic  x(48) value
                    "dcp zp3 Tabella sottogruppi merceologici prod.  ".
                   15  filler             pic  x(48) value
                    "gep zpg Tabella codici di pagamento             ".
                   15  filler             pic  x(48) value
                    "dcp zps Tabella classificatori stat. prodotti   ".
                   15  filler             pic  x(48) value
                    "age zpv Tabelle categorie provvigionali         ".
                   15  filler             pic  x(48) value
                    "age zpx Tabella provvigioni incrociate          ".
                   15  filler             pic  x(48) value
                    "cge zpu Tabelle per gestione Modello Pag. Un. NI".
                   15  filler             pic  x(48) value
                    "orc zro Tabella responsabili ordini clienti     ".
                   15  filler             pic  x(48) value
                    "dps zs1 Tabella classi merceologiche semilavor. ".
                   15  filler             pic  x(48) value
                    "dps zs2 Tabella gruppi merceologici semilavorati".
                   15  filler             pic  x(48) value
                    "dps zs3 Tabella sottogruppi merceologici semil. ".
                   15  filler             pic  x(48) value
                    "ods zsa Tabella tipi annotazione per spedizioni ".
                   15  filler             pic  x(48) value
                    "gep zsb Tabella calcolo aritmetico bolli su eff.".
                   15  filler             pic  x(48) value
                    "ods zsc Tabella tipi movimento ordini spedizione".
                   15  filler             pic  x(48) value
                    "dcc zsf Tabella definizione voci spese fatturaz.".
                   15  filler             pic  x(48) value
                    "cge zsk Scheletri per contabilita'              ".
                   15  filler             pic  x(48) value
                    "gep zso Tabella tipi di sollecito               ".
                   15  filler             pic  x(48) value
                    "dps zss Tabella classificatori stat. semilavor. ".
                   15  filler             pic  x(48) value
                    "dcc zst Tabella voci statistiche clienti        ".
                   15  filler             pic  x(48) value
                    "dcc zsx Tabella sconti incrociati               ".
                   15  filler             pic  x(48) value
                    "bil ztb Tabella tipi di bilancio                ".
                   15  filler             pic  x(48) value
                    "rda ztr Tabella codici tributi                  ".
                   15  filler             pic  x(48) value
                    "fab zts Tabella tipi stagionalita' fabbisogni   ".
                   15  filler             pic  x(48) value
                    "dcp ztv Tabella tipi variante per prodotti      ".
                   15  filler             pic  x(48) value
                    "dcp zum Tabella unita' di misura                ".
                   15  filler             pic  x(48) value
                    "mtv zv1 Tabella classi merceologiche mat. vari  ".
                   15  filler             pic  x(48) value
                    "mtv zv2 Tabella gruppi merceologici mat. vari   ".
                   15  filler             pic  x(48) value
                    "mtv zv3 Tab. sottogruppi merceologici mat. vari ".
                   15  filler             pic  x(48) value
                    "dcc zvf Tabella voci descrittive fatturazione   ".
                   15  filler             pic  x(48) value
                    "dcc zvl Tabella valute                          ".
                   15  filler             pic  x(48) value
                    "mtv zvs Tabella classificatori stat. mat. vari  ".
               10  w-sgr-nam-tbr redefines
                   w-sgr-nam-tbl.
                   15  w-sgr-nam-ele occurs 197.
                       20  w-sgr-nam-atb  pic  x(03)                  .
                       20  filler         pic  x(01)                  .
                       20  w-sgr-nam-ntb  pic  x(04)                  .
                       20  w-sgr-nam-dtb  pic  x(40)                  .
