       Identification Division.
       Program-Id.                                 mioerr             .
      *================================================================*
      *                                                                *
      * Modulo per il trattamento degli errori di i-o su tutti gli ar- *
      * chivi ad esclusione di quelli del sottosistema di stampa, ov-  *
      * vero ad esclusione degli errori di i-o su [stp] e [pfc]        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * Utilizza come link-area l'area di interfaccia segreteria : s   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Parametri in ingresso :                                        *
      *                                                                *
      *  - s-nam : file name                                           *
      *                                                                *
      *  - s-pat : file pathname                                       *
      *                                                                *
      *  - s-sts : file status                                         *
      *                                                                *
      *  - s-fun : tipo funzionamento                                  *
      *            - F : Foreground                                    *
      *            - B : Background                                    *
      *                                                                *
      *  - s-azi : codice azienda                                      *
      *                                                                *
      *  - s-ter : codice terminale                                    *
      *                                                                *
      *  - s-ute : codice utente                                       *
      *                                                                *
      *  - s-sap : sistema applicativo                                 *
      *                                                                *
      *  - s-arg : area gestionale                                     *
      *                                                                *
      *  - s-set : settore gestionale                                  *
      *                                                                *
      *  - s-fas : fase gestionale                                     *
      *                                                                *
      *  - s-pro : sigla programma                                     *
      *                                                                *
      *                                                                *
      *    ---------------------------------------------------------   *
      *                                                                *
      *    Questo modulo puo' essere chiamato esclusivamente dal mo-   *
      *    dulo di segreteria "msegrt", di cui rappresenta un'appen-   *
      *    dice non residente al solo scopo di non aumentare la memo-  *
      *    ria residente impegnata da "msegrt" stesso.                 *
      *                                                                *
      *================================================================*

      ******************************************************************
       Environment Division.
      ******************************************************************

      *================================================================*
       Configuration Section.
      *================================================================*

       Source-Computer.     w-i-p-NdK-PD .
       Object-Computer.     w-i-p-NdK-PD .

       Special-Names.       Decimal-Point is comma .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mvideo"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mmessg"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/m"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "miosts"  *
      *    *-----------------------------------------------------------*
       01  c.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  c-nam                      pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  c-pat                      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  c-sts                      pic  x(02)                  .

      *    *===========================================================*
      *    * Work per salvataggio valori provenienti da segreteria     *
      *    *-----------------------------------------------------------*
       01  w-sgr.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  w-sgr-nam                  pic  x(04)                  .
           05  w-sgr-nam-des              pic  x(40)                  .
           05  w-sgr-nam-are              pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  w-sgr-pat                  pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  w-sgr-sts                  pic  x(02)                  .
           05  w-sgr-sts-des              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Estensione al file status                             *
      *        *-------------------------------------------------------*
           05  w-sgr-ext                  pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo funzionamento                                    *
      *        *-------------------------------------------------------*
           05  w-sgr-fun                  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Codice azienda                                        *
      *        *-------------------------------------------------------*
           05  w-sgr-azi                  pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Codice terminale                                      *
      *        *-------------------------------------------------------*
           05  w-sgr-ter                  pic  x(08)                  .
      *        *-------------------------------------------------------*
      *        * Codice utente                                         *
      *        *-------------------------------------------------------*
           05  w-sgr-ute                  pic  x(08)                  .
      *        *-------------------------------------------------------*
      *        * Sistema applicativo                                   *
      *        *-------------------------------------------------------*
           05  w-sgr-sap                  pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Area gestionale                                       *
      *        *-------------------------------------------------------*
           05  w-sgr-arg                  pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  w-sgr-set                  pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  w-sgr-fas                  pic  x(06)                  .
      *        *-------------------------------------------------------*
      *        * Sigla programma                                       *
      *        *-------------------------------------------------------*
           05  w-sgr-pro                  pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Tabella di decodifica fasi                            *
      *        *-------------------------------------------------------*
           05  w-sgr-nam-rdf.
               10  w-sgr-nam-max          pic  9(03)       value 191  .
               10  w-sgr-nam-lun          pic  9(02)       value 48   .
               10  w-sgr-nam-ctr          pic  9(03)                  .
               10  w-sgr-nam-tbl.
                   15  filler             pic  x(48) value
                    "DCF aaf Condizioni di acquisto fornitore/prod.  ".
                   15  filler             pic  x(48) value
                    "DCF aaq Condizioni generali di acquisto prodotto".
                   15  filler             pic  x(48) value
                    "AZI ada Azienda - dati anagrafici               ".
                   15  filler             pic  x(48) value
                    "AGE age Anagrafica agenti                       ".
                   15  filler             pic  x(48) value
                    "AGE ags Legami tra agenti e super-agenti        ".
                   15  filler             pic  x(48) value
                    "CGE ali Riepilogo movimenti per allegati Iva    ".
                   15  filler             pic  x(48) value
                    "ABI axc Circuiti interbancari                   ".
                   15  filler             pic  x(48) value
                    "ABI axi Istituti di credito                     ".
                   15  filler             pic  x(48) value
                    "ABI axs Sportelli degli istituti di credito     ".
                   15  filler             pic  x(48) value
                    "SCF bef Anagrafica banche estere                ".
                   15  filler             pic  x(48) value
                    "BFO bfr Bolle fornitori, righe                  ".
                   15  filler             pic  x(48) value
                    "BFO bft Bolle fornitori, testate                ".
                   15  filler             pic  x(48) value
                    "BFO bfx Bolle fornitori, estensioni a testate   ".
                   15  filler             pic  x(48) value
                    "BOL bie Bolle clienti, evasioni                 ".
                   15  filler             pic  x(48) value
                    "BOL bir Bolle clienti, righe                    ".
                   15  filler             pic  x(48) value
                    "BOL bit Bolle clienti, testate                  ".
                   15  filler             pic  x(48) value
                    "BOL bix Bolle clienti, estensioni alle testate  ".
                   15  filler             pic  x(48) value
                    "GEP cbp Anagrafica casse, banche, c/c postali   ".
                   15  filler             pic  x(48) value
                    "GEP ccc Anagrafica controllo crediti clienti    ".
                   15  filler             pic  x(48) value
                    "CGE cdb Anagrafica classificatori di bilancio   ".
                   15  filler             pic  x(48) value
                    "GEP cec Controllo esposizione clienti           ".
                   15  filler             pic  x(48) value
                    "CDP cdp Commesse di produzione, testata e riga  ".
                   15  filler             pic  x(48) value
                    "CGE cli Anagrafica clienti contabile            ".
                   15  filler             pic  x(48) value
                    "DCC dcc Anagrafica clienti commerciale          ".
                   15  filler             pic  x(48) value
                    "DCF dcf Anagrafica fornitori commerciale        ".
                   15  filler             pic  x(48) value
                    "DCC dcm Anagrafica clienti per il marketing     ".
                   15  filler             pic  x(48) value
                    "DCP dcp Anagrafica prodotti                     ".
                   15  filler             pic  x(48) value
                    "DCC dcx Annotazioni clienti commerciale         ".
                   15  filler             pic  x(48) value
                    "GEP ddp Distinte di presentazione effetti       ".
                   15  filler             pic  x(48) value
                    "DCF dfx Annotazioni fornitori commerciale       ".
                   15  filler             pic  x(48) value
                    "DPM dpm Anagrafica materie prime                ".
                   15  filler             pic  x(48) value
                    "DPS dps Anagrafica semilavorati                 ".
                   15  filler             pic  x(48) value
                    "FAB fbs Parametri per gestione sottoscorta      ".
                   15  filler             pic  x(48) value
                    "FFO ffr Fatture fornitori, righe                ".
                   15  filler             pic  x(48) value
                    "FFO fft Fatture fornitori, testate              ".
                   15  filler             pic  x(48) value
                    "FFO ffx Fatture fornitori, estensioni a testate ".
                   15  filler             pic  x(48) value
                    "FAT fir Fatture a clienti, righe                ".
                   15  filler             pic  x(48) value
                    "FAT fit Fatture a clienti, testate              ".
                   15  filler             pic  x(48) value
                    "FAT fix Fatture a clienti, estensioni a testate ".
                   15  filler             pic  x(48) value
                    "CGE fnt Anagrafica fornitori contabile          ".
                   15  filler             pic  x(48) value
                    "GEP gep Personalizzazioni S.I.A. per portafoglio".
                   15  filler             pic  x(48) value
                    "AGE gpc Conteggi provvigionali                  ".
                   15  filler             pic  x(48) value
                    "AGE gpm Maturazioni su conteggi provvigionali   ".
                   15  filler             pic  x(48) value
                    "AZI gva Anagrafica veicoli aziendali            ".
                   15  filler             pic  x(48) value
                    "AZI gvs Scadenze relative ai veicoli aziendali  ".
                   15  filler             pic  x(48) value
                    "GEO gxc Anagrafica comuni, frazioni, e localita'".
                   15  filler             pic  x(48) value
                    "GEO gxn Anagrafica nazioni                      ".
                   15  filler             pic  x(48) value
                    "GEO gxp Anagrafica provincie                    ".
                   15  filler             pic  x(48) value
                    "GEO gxr Anagrafica regioni                      ".
                   15  filler             pic  x(48) value
                    "GEO gxs Anagrafica stradari                     ".
                   15  filler             pic  x(48) value
                    "IIC iir Movimenti per Iva intrac., righe        ".
                   15  filler             pic  x(48) value
                    "IIC iit Movimenti per Iva intrac., testate      ".
                   15  filler             pic  x(48) value
                    "CGE ivp Riepilogo movimenti dichiarazione Iva   ".
                   15  filler             pic  x(48) value
                    "DCF lfd Prezzi acquisto fornitori storicizzati  ".
                   15  filler             pic  x(48) value
                    "DTP lgr Distinte base, righe distinta           ".
                   15  filler             pic  x(48) value
                    "DTP lgt Distinte base, testate distinta         ".
                   15  filler             pic  x(48) value
                    "DTP lgv Anagrafica sub-distinte base virtuali   ".
                   15  filler             pic  x(48) value
                    "CGE lic Lettere di intento dei clienti          ".
                   15  filler             pic  x(48) value
                    "DCP lsd Prezzi di listino storicizzati per data ".
                   15  filler             pic  x(48) value
                    "DCP lst Prezzi di listino, e netti concordati   ".
                   15  filler             pic  x(48) value
                    "MAG mau Ubicazioni prodotti a magazzino         ".
                   15  filler             pic  x(48) value
                    "MAG mim Archivio di appoggio rilevazioni invent.".
                   15  filler             pic  x(48) value
                    "CGE mgr Primanota, righe                        ".
                   15  filler             pic  x(48) value
                    "CGE mgs Progressivi contabili per sottoconti    ".
                   15  filler             pic  x(48) value
                    "CGE mgt Primanota, testate                      ".
                   15  filler             pic  x(48) value
                    "MAG mmr Movimenti gestione magazzino, righe     ".
                   15  filler             pic  x(48) value
                    "MAG mms Saldi quantita' per gestione magazzino  ".
                   15  filler             pic  x(48) value
                    "MAG mmt Movimenti gestione magazzino, testate   ".
                   15  filler             pic  x(48) value
                    "MAG mmv Saldi valore per gestione magazzino     ".
                   15  filler             pic  x(48) value
                    "MAG mmz Saldi quantita' in conto merce          ".
                   15  filler             pic  x(48) value
                    "CGE moi Primanota, estensione registrazioni Iva ".
                   15  filler             pic  x(48) value
                    "CGE mpu Primanota, Modello di Pagamento Unific. ".
                   15  filler             pic  x(48) value
                    "MTV mtv Anagrafica materiali vari               ".
                   15  filler             pic  x(48) value
                    "XPG num Numerazioni varie                       ".
                   15  filler             pic  x(48) value
                    "GEP obp Estensioni alle scadenze in portafoglio ".
                   15  filler             pic  x(48) value
                    "ORC ocp Assegnazioni a righe ordini clienti     ".
                   15  filler             pic  x(48) value
                    "ORC ocr Ordini clienti, righe                   ".
                   15  filler             pic  x(48) value
                    "ORC oct Ordini clienti, testate                 ".
                   15  filler             pic  x(48) value
                    "ORC ocx Ordini clienti, estensioni alle testate ".
                   15  filler             pic  x(48) value
                    "ORF ofr Ordini fornitori, righe                 ".
                   15  filler             pic  x(48) value
                    "ORF oft Ordini fornitori, testate               ".
                   15  filler             pic  x(48) value
                    "ORF ofx Ordini fornitori, estensioni a testate  ".
                   15  filler             pic  x(48) value
                    "ODS osk Ordini di spedizione, righe Packing List".
                   15  filler             pic  x(48) value
                    "ODS osr Ordini di spedizione clienti, righe     ".
                   15  filler             pic  x(48) value
                    "ODS ost Ordini di spedizione clienti, testate   ".
                   15  filler             pic  x(48) value
                    "ODS osx Ordini di spedizione clienti, estensioni".
                   15  filler             pic  x(48) value
                    "CGE pdc Anagrafica sottoconti piano dei conti   ".
                   15  filler             pic  x(48) value
                    "DCF pdt Anagrafica case produttrici             ".
                   15  filler             pic  x(48) value
                    "DCP pdx Anagrafica estensioni descrizioni prod. ".
                   15  filler             pic  x(48) value
                    "XPG prs Personalizzazioni generali              ".
                   15  filler             pic  x(48) value
                    "RDA raa Archivio anagrafico gestione ritenuta   ".
                   15  filler             pic  x(48) value
                    "RDA ram Movimenti gestione ritenuta d'acconto   ".
                   15  filler             pic  x(48) value
                    "CGE rdb Movimenti di rettifica                  ".
                   15  filler             pic  x(48) value
                    "XPG ref Referenze generali                      ".
                   15  filler             pic  x(48) value
                    "GEP rsc Relazioni tra scadenze debitori         ".
                   15  filler             pic  x(48) value
                    "GEP rsd Riscossioni scadenze debitori           ".
                   15  filler             pic  x(48) value
                    "XPG sce Scelte varie, [obsoleto]                ".
                   15  filler             pic  x(48) value
                    "GEP scr Solleciti a clienti, righe              ".
                   15  filler             pic  x(48) value
                    "GEP sct Solleciti a clienti, testate            ".
                   15  filler             pic  x(48) value
                    "GEP sdb Scadenze in portafoglio                 ".
                   15  filler             pic  x(48) value
                    "SCF sfa Addebiti su ordini di pagamento         ".
                   15  filler             pic  x(48) value
                    "SCF sff Fatture fornitori (da scadenziario)     ".
                   15  filler             pic  x(48) value
                    "SCF sfp Pagamenti registrati                    ".
                   15  filler             pic  x(48) value
                    "SCF sfs Scadenze fornitori                      ".
                   15  filler             pic  x(48) value
                    "SVF svs Set statistiche di vendita sul fatturato".
                   15  filler             pic  x(48) value
                    "XPG tbl Tabelle varie, in via di obsolescenza   ".
                   15  filler             pic  x(48) value
                    "XPG upr User Personal Reminder - promemoria     ".
                   15  filler             pic  x(48) value
                    "BIL vbr Voci di bilancio, righe                 ".
                   15  filler             pic  x(48) value
                    "BIL vbt Voci di bilancio, testate               ".
                   15  filler             pic  x(48) value
                    "BIL vbv Voci di bilancio, valori                ".
                   15  filler             pic  x(48) value
                    "BOL vet Anagrafica vettori                      ".
                   15  filler             pic  x(48) value
                    "DCC vlt Valori dei cambi per le valute estere   ".
                   15  filler             pic  x(48) value
                    "VDP vpr Versamenti da produzione, righe consumi ".
                   15  filler             pic  x(48) value
                    "VDP vpt Versamenti da produzione, testata e riga".
                   15  filler             pic  x(48) value
                    "BFO ybf Tabella tipi movimento per bolle fornit.".
                   15  filler             pic  x(48) value
                    "DCF ybo Tabella addebito spese bollo da fornit. ".
                   15  filler             pic  x(48) value
                    "ORF yca Tabella descrizioni condizioni acquisto ".
                   15  filler             pic  x(48) value
                    "CDP ycp Tabella tipi movimento per commesse pr. ".
                   15  filler             pic  x(48) value
                    "DCF ydf Tabella definizione voci descr. fornit. ".
                   15  filler             pic  x(48) value
                    "FFO yff Tabella tipi movimento fatture fornitori".
                   15  filler             pic  x(48) value
                    "DCF yfp Tabella forme di pagamento da fornitori ".
                   15  filler             pic  x(48) value
                    "DCF yin Tabella addebito spese incasso fornitori".
                   15  filler             pic  x(48) value
                    "ORF yof Tabella tipi movimento ordini fornitori ".
                   15  filler             pic  x(48) value
                    "SCF yop Tabella tipi operazione scadenze fornit.".
                   15  filler             pic  x(48) value
                    "CDP yrc Tabella responsabili per commesse prod. ".
                   15  filler             pic  x(48) value
                    "ORF yro Tabella responsabili per ordini fornit. ".
                   15  filler             pic  x(48) value
                    "DCF ysf Tabella definizione voci spese fatt. for".
                   15  filler             pic  x(48) value
                    "DCF yst Tabella valori delle voci statistiche   ".
                   15  filler             pic  x(48) value
                    "DCF yvf Tabella valori voci descrittive fatt. f.".
                   15  filler             pic  x(48) value
                    "VDP yvp Tabella tipi movimento versamenti prod. ".
                   15  filler             pic  x(48) value
                    "BOL zab Tabella descrizioni aspetto est. beni   ".
                   15  filler             pic  x(48) value
                    "FAT zac Tabella descrizioni addebiti o commenti ".
                   15  filler             pic  x(48) value
                    "BOL zba Tabella descriz. annotazioni pie' bolla ".
                   15  filler             pic  x(48) value
                    "BOL zbi Tabella tipi movimento per bolle clienti".
                   15  filler             pic  x(48) value
                    "DCC zbo Tabella addebito spese bollo in fattura ".
                   15  filler             pic  x(48) value
                    "CGE zcc Tabella causali contabili primanota     ".
                   15  filler             pic  x(48) value
                    "CGE zci Tabella codici Iva                      ".
                   15  filler             pic  x(48) value
                    "RDA zco Tabella tipi contributi gestione riten. ".
                   15  filler             pic  x(48) value
                    "RDA zcr Tabella causali gestione ritenuta d'acc.".
                   15  filler             pic  x(48) value
                    "DCC zcs Tabella categorie di sconto             ".
                   15  filler             pic  x(48) value
                    "BOL zct Tabella causali del trasporto           ".
                   15  filler             pic  x(48) value
                    "ORC zcv Tabella descrizioni condizioni ordini cl".
                   15  filler             pic  x(48) value
                    "DCC zdf Tabella definizione voci descrittive    ".
                   15  filler             pic  x(48) value
                    "FAT zfi Tabella tipi movimento per fatturazione ".
                   15  filler             pic  x(48) value
                    "DCC zfp Tabella forme di pagamento per clienti  ".
                   15  filler             pic  x(48) value
                    "DCC zin Tabella addebito spese incasso fattura  ".
                   15  filler             pic  x(48) value
                    "DCC zkc Tabella codici marketing per clienti    ".
                   15  filler             pic  x(48) value
                    "DCC zln Tabella per la definizione codici lingua".
                   15  filler             pic  x(48) value
                    "DCP zls Tabella tipi di listino per fatturazione".
                   15  filler             pic  x(48) value
                    "DPM zm1 Tabella classi merceologiche materie pr.".
                   15  filler             pic  x(48) value
                    "DPM zm2 Tabella gruppi merceologici materie pr. ".
                   15  filler             pic  x(48) value
                    "DPM zm3 Tabella sottogruppi merceologici mat. pr".
                   15  filler             pic  x(48) value
                    "MAG zmc Tabella causali gestione magazzino      ".
                   15  filler             pic  x(48) value
                    "MAG zmd Tabella dislocazioni per dipendenza     ".
                   15  filler             pic  x(48) value
                    "MAG zmm Tabella tipi conto merce                ".
                   15  filler             pic  x(48) value
                    "DPM zms Tabella classificatori stat. materie pr.".
                   15  filler             pic  x(48) value
                    "MAG zmu Tabella tipi ubicazione                 ".
                   15  filler             pic  x(48) value
                    "ORC zoc Tabella tipi movimento ordini clienti   ".
                   15  filler             pic  x(48) value
                    "GEP zop Tabella tipi operazione gestione portaf.".
                   15  filler             pic  x(48) value
                    "AZI zos Tabella filtri di ordinamento selezione ".
                   15  filler             pic  x(48) value
                    "DCP zp1 Tabella classi merceologiche prodotti   ".
                   15  filler             pic  x(48) value
                    "DCP zp2 Tabella gruppi merceologici per prodotti".
                   15  filler             pic  x(48) value
                    "DCP zp3 Tabella sottogruppi merceologici prod.  ".
                   15  filler             pic  x(48) value
                    "GEP zpg Tabella codici di pagamento             ".
                   15  filler             pic  x(48) value
                    "DCP zps Tabella classificatori stat. prodotti   ".
                   15  filler             pic  x(48) value
                    "AGE zpv Tabelle categorie provvigionali         ".
                   15  filler             pic  x(48) value
                    "AGE zpx Tabella provvigioni incrociate          ".
                   15  filler             pic  x(48) value
                    "CGE zpu Tabelle per gestione Modello Pag. Un. NI".
                   15  filler             pic  x(48) value
                    "ORC zro Tabella responsabili ordini clienti     ".
                   15  filler             pic  x(48) value
                    "DPS zs1 Tabella classi merceologiche semilavor. ".
                   15  filler             pic  x(48) value
                    "DPS zs2 Tabella gruppi merceologici semilavorati".
                   15  filler             pic  x(48) value
                    "DPS zs3 Tabella sottogruppi merceologici semil. ".
                   15  filler             pic  x(48) value
                    "ODS zsa Tabella tipi annotazione per spedizioni ".
                   15  filler             pic  x(48) value
                    "GEP zsb Tabella calcolo aritmetico bolli su eff.".
                   15  filler             pic  x(48) value
                    "ODS zsc Tabella tipi movimento ordini spedizione".
                   15  filler             pic  x(48) value
                    "DCC zsf Tabella definizione voci spese fatturaz.".
                   15  filler             pic  x(48) value
                    "GEP zso Tabella tipi di sollecito               ".
                   15  filler             pic  x(48) value
                    "DPS zss Tabella classificatori stat. semilavor. ".
                   15  filler             pic  x(48) value
                    "DCC zst Tabella voci statistiche clienti        ".
                   15  filler             pic  x(48) value
                    "DCC zsx Tabella sconti incrociati               ".
                   15  filler             pic  x(48) value
                    "BIL ztb Tabella tipi di bilancio                ".
                   15  filler             pic  x(48) value
                    "RDA ztr Tabella codici tributi                  ".
                   15  filler             pic  x(48) value
                    "FAB zts Tabella tipi stagionalita' fabbisogni   ".
                   15  filler             pic  x(48) value
                    "DCP ztv Tabella tipi variante per prodotti      ".
                   15  filler             pic  x(48) value
                    "DCP zum Tabella unita' di misura                ".
                   15  filler             pic  x(48) value
                    "MTV zv1 Tabella classi merceologiche mat. vari  ".
                   15  filler             pic  x(48) value
                    "MTV zv2 Tabella gruppi merceologici mat. vari   ".
                   15  filler             pic  x(48) value
                    "MTV zv3 Tab. sottogruppi merceologici mat. vari ".
                   15  filler             pic  x(48) value
                    "DCC zvf Tabella voci descrittive fatturazione   ".
                   15  filler             pic  x(48) value
                    "DCC zvl Tabella valute                          ".
                   15  filler             pic  x(48) value
                    "MTV zvs Tabella classificatori stat. mat. vari  ".
               10  w-sgr-nam-tbr redefines
                   w-sgr-nam-tbl.
                   15  w-sgr-nam-ele occurs 191.
                       20  w-sgr-nam-atb  pic  x(03)                  .
                       20  filler         pic  x(01)                  .
                       20  w-sgr-nam-ntb  pic  x(04)                  .
                       20  w-sgr-nam-dtb  pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Tabella di decodifica errori                          *
      *        *-------------------------------------------------------*
           05  w-sgr-err-rdf.
               10  w-sgr-err-max          pic  9(02)       value 26   .
               10  w-sgr-err-lun          pic  9(02)       value 43   .
               10  w-sgr-err-ctr          pic  9(02)                  .
               10  w-sgr-err-tbl.
                   15  filler             pic  x(43) value
                    "00 Nessun errore                           "     .
                   15  filler             pic  x(43) value
                    "02 Chiave duplicata                        "     .
                   15  filler             pic  x(43) value
                    "10 Raggiunta la fine dell'archivio         "     .
                   15  filler             pic  x(43) value
                    "21 Record non in ordine sequenziale        "     .
                   15  filler             pic  x(43) value
                    "22 Record gia' esistente                   "     .
                   15  filler             pic  x(43) value
                    "23 Record non esistente                    "     .
                   15  filler             pic  x(43) value
                    "30 Errore hardware - permanente            "     .
                   15  filler             pic  x(43) value
                    "34 Errore hardware - dischi pieni          "     .
                   15  filler             pic  x(43) value
                    "39 Archivio con struttura modificata       "     .
                   15  filler             pic  x(43) value
                    "41 Archivio aperto piu' di una volta       "     .
                   15  filler             pic  x(43) value
                    "46 Lettura sequenziale errata              "     .
                   15  filler             pic  x(43) value
                    "47 Archivio non aperto (NOT OPEN)          "     .
                   15  filler             pic  x(43) value
                    "91 Archivio bloccato da altri utenti       "     .
                   15  filler             pic  x(43) value
                    "91 Archivio bloccato da altri utenti       "     .
                   15  filler             pic  x(43) value
                    "92 Archivio non aperto per la lettura      "     .
                   15  filler             pic  x(43) value
                    "93 Archivio bloccato da altri utenti       "     .
                   15  filler             pic  x(43) value
                    "94 Troppi archivi aperti                   "     .
                   15  filler             pic  x(43) value
                    "97 Troppi archivi bloccati                 "     .
                   15  filler             pic  x(43) value
                    "98 Archivio rovinato per chiusura anomala  "     .
                   15  filler             pic  x(43) value
                    "9A Memoria insufficiente                   "     .
                   15  filler             pic  x(43) value
                    "9B Archivio con indici corrotti            "     .
                   15  filler             pic  x(43) value
                    "9C Archivio con accesso ristretto (CHMOD)  "     .
                   15  filler             pic  x(43) value
                    "9D Archivio con nome non corretto          "     .
                   15  filler             pic  x(43) value
                    "9E Archivio con record troppo lungo        "     .
                   15  filler             pic  x(43) value
                    "9F Pagina piena (sequenziale)              "     .
                   15  filler             pic  x(43) value
                    "** ERRORE FATALE                           "     .
               10  w-sgr-err-tbr redefines
                   w-sgr-err-tbl.
                   15  w-sgr-err-ele occurs 26.
                       20  w-sgr-err-ctb  pic  x(02)                  .
                       20  filler         pic  x(01)                  .
                       20  w-sgr-err-dtb  pic  x(40)                  .

      *    *===========================================================*
      *    * Work per visualizzazione errore in foreground             *
      *    *-----------------------------------------------------------*
       01  w-err-frg.
      *        *-------------------------------------------------------*
      *        * Comodo per risposta data dall'operatore               *
      *        *-------------------------------------------------------*
           05  w-err-frg-acc              pic  x(02)                  .

      *    *===========================================================*
      *    * Work per visualizzazione errore in background             *
      *    *-----------------------------------------------------------*
       01  w-err-bkg.
      *        *-------------------------------------------------------*
      *        * Comodo per codice azienda                             *
      *        *-------------------------------------------------------*
           05  w-err-bkg-azi              pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per codice terminale                           *
      *        *-------------------------------------------------------*
           05  w-err-bkg-ter              pic  x(08)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per codice utente                              *
      *        *-------------------------------------------------------*
           05  w-err-bkg-ute              pic  x(08)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per sistema applicativo                        *
      *        *-------------------------------------------------------*
           05  w-err-bkg-sap              pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per area gestionale                            *
      *        *-------------------------------------------------------*
           05  w-err-bkg-arg              pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per settore gestionale                         *
      *        *-------------------------------------------------------*
           05  w-err-bkg-set              pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per fase gestionale                            *
      *        *-------------------------------------------------------*
           05  w-err-bkg-fas              pic  x(06)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per programma                                  *
      *        *-------------------------------------------------------*
           05  w-err-bkg-pro              pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per file name                                  *
      *        *-------------------------------------------------------*
           05  w-err-bkg-nam              pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per file pathname                              *
      *        *-------------------------------------------------------*
           05  w-err-bkg-pat              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per file status                                *
      *        *-------------------------------------------------------*
           05  w-err-bkg-sts              pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per string del parametro                       *
      *        *-------------------------------------------------------*
           05  w-err-bkg-cps              pic  x(40)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using s                      .
      ******************************************************************

      *================================================================*
      *       Main program                                             *
      *----------------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Salvataggio valori provenienti da segreteria    *
      *              *-------------------------------------------------*
           perform   sav-val-sgr-000      thru sav-val-sgr-999        .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se foreground o background *
      *              *-------------------------------------------------*
           if        w-sgr-fun            =    "F"
                     go to main-200
           else      go to main-400.
       main-200.
      *              *-------------------------------------------------*
      *              * Se funzionamento in foreground                  *
      *              *-------------------------------------------------*
           perform   vis-err-frg-000      thru vis-err-frg-999        .
           go to     main-999.
       main-400.
      *              *-------------------------------------------------*
      *              * Se funzionamento in background                  *
      *              *-------------------------------------------------*
           perform   vis-err-bkg-000      thru vis-err-bkg-999        .
       main-999.
           exit      program                                          .

      *    *===========================================================*
      *    * Salvataggio valori provenienti da segreteria              *
      *    *-----------------------------------------------------------*
       sav-val-sgr-000.
      *              *-------------------------------------------------*
      *              * File name                                       *
      *              *-------------------------------------------------*
           move      s-nam                to   w-sgr-nam              .
      *              *-------------------------------------------------*
      *              * File pathname                                   *
      *              *-------------------------------------------------*
           move      s-pat                to   w-sgr-pat              .
      *              *-------------------------------------------------*
      *              * File status                                     *
      *              *-------------------------------------------------*
           move      s-sts                to   w-sgr-sts              .
      *              *-------------------------------------------------*
      *              * Tipo funzionamento                              *
      *              *-------------------------------------------------*
           move      s-fun                to   w-sgr-fun              .
      *              *-------------------------------------------------*
      *              * Codice azienda                                  *
      *              *-------------------------------------------------*
           move      s-azi                to   w-sgr-azi              .
      *              *-------------------------------------------------*
      *              * Codice terminale                                *
      *              *-------------------------------------------------*
           move      s-ter                to   w-sgr-ter              .
      *              *-------------------------------------------------*
      *              * Codice utente                                   *
      *              *-------------------------------------------------*
           move      s-ute                to   w-sgr-ute              .
      *              *-------------------------------------------------*
      *              * Sistema applicativo                             *
      *              *-------------------------------------------------*
           move      s-sap                to   w-sgr-sap              .
      *              *-------------------------------------------------*
      *              * Area gestionale                                 *
      *              *-------------------------------------------------*
           move      s-arg                to   w-sgr-arg              .
      *              *-------------------------------------------------*
      *              * Settore gestionale                              *
      *              *-------------------------------------------------*
           move      s-set                to   w-sgr-set              .
      *              *-------------------------------------------------*
      *              * Fase gestionale                                 *
      *              *-------------------------------------------------*
           move      s-fas                to   w-sgr-fas              .
      *              *-------------------------------------------------*
      *              * Sigla programma                                 *
      *              *-------------------------------------------------*
           move      s-pro                to   w-sgr-pro              .
      *              *-------------------------------------------------*
      *              * Estensione al file status                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo del modulo miosts per ottenere le  *
      *                  * informazioni relative all'ultimo file che   *
      *                  * ha provocato un errore di i-o               *
      *                  *---------------------------------------------*
           move      low-values           to   c-sts                  .
           call      "swd/mod/prg/obj/miosts"
                                         using c                      .
      *                  *---------------------------------------------*
      *                  * Se file name e pathname differiscono da     *
      *                  * quelli passati dalla segreteria : uscita    *
      *                  * con estensione a high-values                *
      *                  *---------------------------------------------*
           if        c-nam                not  = w-sgr-nam or
                     c-pat                not  = w-sgr-pat
                     move  high-values    to   w-sgr-ext
                     go to sav-val-sgr-999.
      *                  *---------------------------------------------*
      *                  * Altrimenti si memorizza il codice di errore *
      *                  * cobol reale                                 *
      *                  *---------------------------------------------*
           move      c-sts                to   w-sgr-sts              .
      *                  *---------------------------------------------*
      *                  * Richiamo del modulo miosts per ottenere le  *
      *                  * informazioni relative all'estensione al co- *
      *                  * dice di errore relativo all ultimo file che *
      *                  * ha provocato un errore di i-o               *
      *                  *---------------------------------------------*
           move      high-values          to   c-sts                  .
           call      "swd/mod/prg/obj/miosts"
                                         using c                      .
      *                  *---------------------------------------------*
      *                  * Se estensione a high-values : uscita con    *
      *                  * estensione a high-values                    *
      *                  *---------------------------------------------*
           if        c-sts                =    high-values
                     move  high-values    to   w-sgr-ext
                     go to sav-val-sgr-999.
      *                  *---------------------------------------------*
      *                  * Altrimenti si memorizza l'estensione al co- *
      *                  * dice di errore cobol reale                  *
      *                  *---------------------------------------------*
           move      c-sts                to   w-sgr-ext              .
       sav-val-sgr-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione errore se funzionamento in foreground     *
      *    *-----------------------------------------------------------*
       vis-err-frg-000.
      *              *-------------------------------------------------*
      *              * Save video                                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Box vuoto                                       *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      21                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-err-frg-100.
      *              *-------------------------------------------------*
      *              * Messaggio interno al box                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sopralineatura titolo                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      47                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      17                   to   v-pos                  .
           move      all "*"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Titolo                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      47                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      17                   to   v-pos                  .
           move      "*   ERRORE DURANTE L'ACCESSO AD UN ARCHIVIO   *"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Sottolineatura titolo                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      47                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      17                   to   v-pos                  .
           move      all "*"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Nome archivio                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      25                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Nome archivio ..........:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      29                   to   v-pos                  .
      *
           move      spaces               to   v-alf                  .
           if        w-sgr-nam            not  = high-values
                     string "[" delimited by   size
                            w-sgr-nam
                                delimited by   spaces
                            "]" delimited by   size
                                          into v-alf                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Decodifica nome archivio                *
      *                      *-----------------------------------------*
           perform   vis-err-frg-dna-000  thru vis-err-frg-dna-999    .
      *                      *-----------------------------------------*
      *                      * Descrizione archivio                    *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-sgr-nam-des        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Nome area                               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      76                   to   v-pos                  .
           move      w-sgr-nam-are        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Identificatore archivio                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      25                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Percorso archivio ......:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      29                   to   v-pos                  .
           move      w-sgr-pat            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Codice di errore                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      25                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Codice errore ..........:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      29                   to   v-pos                  .
           move      w-sgr-sts            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Estensione                              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      31                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           if        w-sgr-ext            not  = high-values
                     string "," delimited by   size
                            w-sgr-ext
                                delimited by   size
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Decodifica errore                       *
      *                      *-----------------------------------------*
           perform   vis-err-frg-dce-000  thru vis-err-frg-dce-999    .
      *                      *-----------------------------------------*
      *                      * Descrizione errore                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-sgr-sts-des        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-err-frg-180.
      *                  *---------------------------------------------*
      *                  * Literal per presa visione                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      36                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      "Digitare 'OK' per presa visione :   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Sopralineatura nota bene                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Nota bene                                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "PER QUESTA TIPOLOGIA DI ERRORE E' INDISPENSABILE C
      -              "ONTATTARE L'ASSISTENZA !  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-err-frg-200.
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore di default per la risposta           *
      *                  *---------------------------------------------*
           move      spaces               to   w-err-frg-acc          .
       vis-err-frg-300.
      *                  *---------------------------------------------*
      *                  * Accettazione                                *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      77                   to   v-pos                  .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-err-frg-acc        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Salvataggio valore impostato                *
      *                  *---------------------------------------------*
           move      v-alf                to   w-err-frg-acc          .
      *                  *---------------------------------------------*
      *                  * Deviazione socondo il tasto funzione        *
      *                  *---------------------------------------------*
           if        v-key                =    spaces
                     go to vis-err-frg-400
           else if   v-key                =    "DO  " or
                     v-key                =    "EXIT"
                     go to vis-err-frg-500
           else if   v-key                =    "FIND"
                     go to vis-err-frg-600
           else      go to vis-err-frg-300.
       vis-err-frg-400.
      *                  *---------------------------------------------*
      *                  * Se impostazione con Return                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se valore non Ok : reimpostazione       *
      *                      *-----------------------------------------*
           if        w-err-frg-acc        not  = "OK"
                     go to vis-err-frg-300.
      *                      *-----------------------------------------*
      *                      * Altrimenti : Restore ed uscita          *
      *                      *-----------------------------------------*
           go to     vis-err-frg-900.
       vis-err-frg-500.
      *                  *---------------------------------------------*
      *                  * Se Do o Exit                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Restore ed uscita                       *
      *                      *-----------------------------------------*
           go to     vis-err-frg-900.
       vis-err-frg-600.
      *                  *---------------------------------------------*
      *                  * Se Find                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo del modulo di interrogazione   *
      *                      * sui codici di errore                    *
      *                      *-----------------------------------------*
           call      "swd/mod/prg/obj/mioerh"                         .
      *                      *-----------------------------------------*
      *                      * Cancellazione del modulo                *
      *                      *-----------------------------------------*
           cancel    "swd/mod/prg/obj/mioerh"                         .
      *                      *-----------------------------------------*
      *                      * Rientro ad accettazione                 *
      *                      *-----------------------------------------*
           go to     vis-err-frg-200.
       vis-err-frg-900.
      *              *-------------------------------------------------*
      *              * Restore video                                   *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-err-frg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione errore se funzionamento in foreground     *
      *    *                                                           *
      *    * Decodifica nome archivio - aggiornata 16/12/02            *
      *    *-----------------------------------------------------------*
       vis-err-frg-dna-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      "Archivio di sistema                     "
                                          to   w-sgr-nam-des          .
           move      spaces               to   w-sgr-nam-are          .
       vis-err-frg-dna-100.
      *              *-------------------------------------------------*
      *              * Se archivio personalizzato                      *
      *              *-------------------------------------------------*
           if        w-sgr-nam
                    (01 : 01)             =    "h"
                     move  "Archivio personalizzato                 "
                                          to   w-sgr-nam-des
                     move  "SUM"          to   w-sgr-nam-are
                     go to vis-err-frg-dna-900.
       vis-err-frg-dna-200.
      *              *-------------------------------------------------*
      *              * Scansione tabella                               *
      *              *-------------------------------------------------*
           move      zero                 to   w-sgr-nam-ctr          .
       vis-err-frg-dna-220.
           add       1                    to   w-sgr-nam-ctr          .
           if        w-sgr-nam-ctr        >    w-sgr-nam-max
                     go to vis-err-frg-dna-900.
           if        w-sgr-nam-ntb
                    (w-sgr-nam-ctr)       =    w-sgr-nam
                     move  w-sgr-nam-dtb
                          (w-sgr-nam-ctr) to   w-sgr-nam-des
                     move  w-sgr-nam-atb
                          (w-sgr-nam-ctr) to   w-sgr-nam-are
                     go to vis-err-frg-dna-900.
           go to     vis-err-frg-dna-220.
       vis-err-frg-dna-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-err-frg-dna-999.
       vis-err-frg-dna-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione errore se funzionamento in foreground     *
      *    *                                                           *
      *    * Decodifica errore - aggiornata 16/12/02                   *
      *    *-----------------------------------------------------------*
       vis-err-frg-dce-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      "Errore indefinito                       "
                                          to   w-sgr-sts-des          .
       vis-err-frg-dce-200.
      *              *-------------------------------------------------*
      *              * Scansione tabella                               *
      *              *-------------------------------------------------*
           move      zero                 to   w-sgr-err-ctr          .
       vis-err-frg-dce-220.
           add       1                    to   w-sgr-err-ctr          .
           if        w-sgr-err-ctr        >    w-sgr-err-max
                     go to vis-err-frg-dce-900.
           if        w-sgr-err-ctb
                    (w-sgr-err-ctr)       =    w-sgr-sts
                     move  w-sgr-err-dtb
                          (w-sgr-err-ctr) to   w-sgr-sts-des
                     go to vis-err-frg-dce-900.
           go to     vis-err-frg-dce-220.
       vis-err-frg-dce-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-err-frg-dce-999.
       vis-err-frg-dce-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione errore se funzionamento in background     *
      *    *-----------------------------------------------------------*
       vis-err-bkg-000.
      *              *-------------------------------------------------*
      *              * Emissione messaggio di errore                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Linea 01                                    *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      "+-------------------------------------------------
      -              "-----------------------------+"
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 02                                    *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      "|                   Errore durante l' accesso ad u
      -              "n archivio                   |"
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 03                                    *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      "|                                                 
      -              "                             |"
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 04                                    *
      *                  *---------------------------------------------*
           move      w-sgr-azi            to   w-err-bkg-cps          .
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "| Azienda .................... : "
                                delimited by   size
                     w-err-bkg-cps
                                delimited by   size
                     "      |"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 05                                    *
      *                  *---------------------------------------------*
           move      w-sgr-ute            to   w-err-bkg-cps          .
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "| Utente ..................... : "
                                delimited by   size
                     w-err-bkg-cps
                                delimited by   size
                     "      |"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 06                                    *
      *                  *---------------------------------------------*
           move      w-sgr-ter            to   w-err-bkg-cps          .
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "| Terminale .................. : "
                                delimited by   size
                     w-err-bkg-cps
                                delimited by   size
                     "      |"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 07                                    *
      *                  *---------------------------------------------*
           move      w-sgr-sap            to   w-err-bkg-cps          .
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "| Sistema applicativo ........ : "
                                delimited by   size
                     w-err-bkg-cps
                                delimited by   size
                     "      |"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 08                                    *
      *                  *---------------------------------------------*
           move      w-sgr-arg            to   w-err-bkg-cps          .
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "| Area gestionale ............ : "
                                delimited by   size
                     w-err-bkg-cps
                                delimited by   size
                     "      |"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 09                                    *
      *                  *---------------------------------------------*
           move      w-sgr-set            to   w-err-bkg-cps          .
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "| Settore gestionale ......... : "
                                delimited by   size
                     w-err-bkg-cps
                                delimited by   size
                     "      |"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 10                                    *
      *                  *---------------------------------------------*
           move      w-sgr-fas            to   w-err-bkg-cps          .
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "| Fase gestionale ............ : "
                                delimited by   size
                     w-err-bkg-cps
                                delimited by   size
                     "      |"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 11                                    *
      *                  *---------------------------------------------*
           move      w-sgr-pro            to   w-err-bkg-cps          .
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "| Programma .................. : "
                                delimited by   size
                     w-err-bkg-cps
                                delimited by   size
                     "      |"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 12                                    *
      *                  *---------------------------------------------*
           move      w-sgr-nam            to   w-err-bkg-cps          .
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "| Nome archivio .............. : "
                                delimited by   size
                     w-err-bkg-cps
                                delimited by   size
                     "      |"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 13                                    *
      *                  *---------------------------------------------*
           move      w-sgr-pat            to   w-err-bkg-cps          .
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "| Identificatore archivio .... : "
                                delimited by   size
                     w-err-bkg-cps
                                delimited by   size
                     "      |"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       vis-err-bkg-500.
      *                  *---------------------------------------------*
      *                  * Linea 14                                    *
      *                  *---------------------------------------------*
           if        w-sgr-ext            =    high-values
                     move  w-sgr-sts      to   w-err-bkg-cps
                     go to vis-err-bkg-550.
           move      spaces               to   w-err-bkg-cps          .
           string    w-sgr-sts
                                delimited by   size
                     ","        delimited by   size
                     w-sgr-ext
                                delimited by   size
                                          into w-err-bkg-cps          .
       vis-err-bkg-550.
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "| Codice interno di errore ... : "
                                delimited by   size
                     w-err-bkg-cps
                                delimited by   size
                     "      |"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 15                                    *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      "|                                                 
      -              "                             |"
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 16                                    *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      "| Prendere nota dei valori sopraesposti e rivolger
      -              "si all'assistenza software.  |"
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 17                                    *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      "+-------------------------------------------------
      -              "-----------------------------+"
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       vis-err-bkg-999.
           exit.
