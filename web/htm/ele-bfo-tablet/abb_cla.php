<?php
/*====================================================================*/
/* Classe             : abb_cla.php                                   */
/*--------------------------------------------------------------------*/
/* Versione originale : 001 del 18/03/25                              */
/* Ultima revisione   : Andrea del 18/03/25                           */
/*--------------------------------------------------------------------*/
/* Descrizione        : Classe che contiene funzioni per la gestione  */
/*                      dei carichi ABB, interfacciandosi con MySQL.  */
/*                                                                    */
/* Funzioni:                                                          */
/* --------                                                           */
/* - getLotti()      -> Recupera i lotti aperti                       */
/* - getOperatori()  -> Recupera gli operatori                        */
/* - insertLotti()   -> Inserisce i lotti nel database                */
/* - getProdotti()   -> Recupera i prodotti di un lotto               */
/* - updateLotto()   -> Aggiorna le informazioni del lotto            */
/*                                                                    */
/*====================================================================*/

/*====================================================================*/
/* Classe : abb_cla (Gestione carichi ABB)                            */
/*--------------------------------------------------------------------*/
class abb_cla {

    /*================================================================*/
    /* Connessione PDO al database                                    */
    /*----------------------------------------------------------------*/
    private $pdo;
    /*================================================================*/
    /* Costruttore della classe                                       */
    /* -------------------------------------------------------------- */
    /* - Inizializza la connessione PDO con il database MySQL         */
    /* - Imposta le opzioni di gestione degli errori e del fetch      */
    /*   di default                                                   */
    /*----------------------------------------------------------------*/
    public function __construct() {
        $dbHost = "localhost";  
        $dbName = "ele";        
        $dbUser = "root";       
        $dbPass = "loppio2";    

        try {
            $dsn = "mysql:host=$dbHost;dbname=$dbName;charset=utf8";
            $this->pdo = new PDO($dsn, $dbUser, $dbPass, array(
                PDO::ATTR_ERRMODE => PDO::ERRMODE_EXCEPTION,
                PDO::ATTR_DEFAULT_FETCH_MODE => PDO::FETCH_ASSOC
            ));
        } catch (PDOException $e) {
            die("Errore di connessione: " . $e->getMessage());
        }
    }

    /*================================================================*/
    /* Funzione : getLotti()                                          */
    /* -------------------------------------------------------------- */
    /* Recupera l'elenco dei lotti aperti dalla tabella `abb_lot`.    */
    /*                                                                */
    /* Output:                                                        */
    /* - Array associativo con i dati dei lotti                       */
    /*----------------------------------------------------------------*/
    public function getLotti() {
        $sql = "SELECT DISTINCT num_prg, rsp_lot FROM abb_lot WHERE (flg_lot IS NULL OR flg_lot <> '#')";
        $stmt = $this->pdo->prepare($sql);
        $stmt->execute();
        return $stmt->fetchAll();
        }

    /*================================================================*/
    /* Funzione : getOperatori()                                      */
    /* -------------------------------------------------------------- */
    /* Recupera l'elenco degli operatori dalla tabella `zrm`.         */
    /*                                                                */
    /* Output:                                                        */
    /* - Array associativo con i dati degli operatori                 */
    /*----------------------------------------------------------------*/
    public function getOperatori() {
        $sql = "SELECT * FROM zrm";
        $stmt = $this->pdo->prepare($sql);
        $stmt->execute();
        return $stmt->fetchAll();
    }

    /*================================================================*/
    /* Funzione : insertLotti()                                       */
    /* -------------------------------------------------------------- */
    /* Inserisce nuovi lotti nella tabella `abb_lot`                  */
    /* e gestisce il progressivo `prt_num` nella tabella `num`.       */
    /*                                                                */
    /* Input:                                                         */
    /* - $prodotti (array) -> Elenco dei prodotti da inserire         */
    /*                                                                */
    /* Output:                                                        */
    /* - Array con lo stato e il messaggio dell'operazione            */
    /*----------------------------------------------------------------*/
    public function insertLotti($prodotti) {
        /*------------------------------------------------------------*/
        /* Controllo se l'array di prodotti è vuoto                   */
        /*------------------------------------------------------------*/
        if (empty($prodotti)) {
            return array("status" => "error", "message" => "Nessun prodotto ricevuto");
        }

        try {
            /*--------------------------------------------------------*/
            /* Inizio della transazione per garantire la coerenza dei */
            /* dati                                                   */
            /*--------------------------------------------------------*/
            $this->pdo->beginTransaction();
            /*--------------------------------------------------------*/
            /* Calcolo del secolo e dell'anno corrente                */
            /*--------------------------------------------------------*/
            $anno_corrente = date("Y");
            $secoloIntero = floor($anno_corrente / 100) + 1;
            $secolo = substr($secoloIntero, -1);
            $anno = substr($anno_corrente, -2);
            $saa_num = $secolo . $anno;
            /*--------------------------------------------------------*/
            /* Controllo dell'esistenza del numero progressivo        */
            /* `prt_num`                                              */
            /*--------------------------------------------------------*/
            $sqlCheck = "SELECT prt_num FROM num WHERE cod_num = 'abblot' AND saa_num = :saa_num";
            $stmtCheck = $this->pdo->prepare($sqlCheck);
            $stmtCheck->bindParam(':saa_num', $saa_num, PDO::PARAM_INT);
            $stmtCheck->execute();
            $row = $stmtCheck->fetch(PDO::FETCH_ASSOC);
            /*--------------------------------------------------------*/
            /* Se esiste già una riga, incremento il progressivo      */
            /*--------------------------------------------------------*/
            if ($row) {
                $prt_num = $row["prt_num"] + 1;
            } else {
                /*----------------------------------------------------*/
                /* Se non esiste, creo una nuova riga con progressivo */
                /* iniziale                                           */
                /*----------------------------------------------------*/
                $prt_num = ($saa_num . "00000001");
                $sqlInsertNum = "INSERT INTO num (cod_num, saa_num, prt_num) VALUES ('abblot', :saa_num, :prt_num)";
                $stmtInsertNum = $this->pdo->prepare($sqlInsertNum);
                $stmtInsertNum->bindParam(':saa_num', $saa_num, PDO::PARAM_INT);
                $stmtInsertNum->bindParam(':prt_num', $prt_num, PDO::PARAM_STR);
                $stmtInsertNum->execute();
            }

            /*--------------------------------------------------------*/
            /* Inserimento dei prodotti nella tabella `abb_lot`       */
            /*--------------------------------------------------------*/
            $sqlInsertLotti = "INSERT INTO abb_lot (num_prg, rsp_lot, alf_pro, qta_rig, flg_lot) VALUES (:id, :rsp_lot, :alf_pro, :qta_rig, NULL)";
            $stmtInsertLotti = $this->pdo->prepare($sqlInsertLotti);

            foreach ($prodotti as $prodotto) {
                $stmtInsertLotti->bindParam(':id', $prt_num, PDO::PARAM_STR);
                $stmtInsertLotti->bindParam(':rsp_lot', $prodotto['rsp_lot'], PDO::PARAM_STR);
                $stmtInsertLotti->bindParam(':alf_pro', $prodotto['alf_pro'], PDO::PARAM_STR);
                $stmtInsertLotti->bindParam(':qta_rig', $prodotto['qta_rig'], PDO::PARAM_STR);
                $stmtInsertLotti->execute();
            }

            /*--------------------------------------------------------*/
            /* Aggiornamento del progressivo `prt_num` in `num`       */
            /*--------------------------------------------------------*/
            $sqlUpdateNum = "UPDATE num SET prt_num = :prt_num WHERE cod_num = 'abblot' AND saa_num = :saa_num";
            $stmtUpdateNum = $this->pdo->prepare($sqlUpdateNum);
            $stmtUpdateNum->bindParam(':prt_num', $prt_num, PDO::PARAM_STR);
            $stmtUpdateNum->bindParam(':saa_num', $saa_num, PDO::PARAM_INT);
            $stmtUpdateNum->execute();
            /*--------------------------------------------------------*/
            /* Conferma della transazione                             */
            /*--------------------------------------------------------*/
            $this->pdo->commit();

            return array("status" => "success", "num_prg" => $prt_num, "message" => "Prodotti inseriti con successo");

        } catch (PDOException $e) {
            /*--------------------------------------------------------*/
            /* In caso di errore, annulla la transazione              */
            /*--------------------------------------------------------*/
            $this->pdo->rollBack();
            return array("status" => "error", "message" => "Errore nell'inserimento: " . $e->getMessage());
        }
    }


    /*================================================================*/
    /* Funzione : getProdottiLotto()                                  */
    /* -------------------------------------------------------------- */
    /* Recupera tutti i prodotti associati a un determinato lotto.    */
    /*                                                                */
    /* Input:                                                         */
    /* - $num_prg (string) -> Numero progressivo del lotto            */
    /*                                                                */
    /* Output:                                                        */
    /* - Array con i prodotti del lotto                               */
    /*----------------------------------------------------------------*/
    public function getProdotti($num_prg) {
        $sql = "SELECT * FROM abb_lot WHERE num_prg = :num_prg";
        $stmt = $this->pdo->prepare($sql);
        $stmt->bindParam(':num_prg', $num_prg, PDO::PARAM_STR);
        $stmt->execute();
        return $stmt->fetchAll();
    }


    /*================================================================*/
    /* Funzione : updateLotto()                                       */
    /* -------------------------------------------------------------- */
    /* Aggiorna la riga del lotto in base alla quantità inserita.     */
    /*                                                                */
    /* Input:                                                         */
    /* - $num_prg   (string) -> Numero progressivo del lotto          */
    /* - $alf_pro   (string) -> Codice prodotto                       */
    /* - $ubi_qta   (numeric) -> Quantità inserita dall'utente        */
    /*                                                                */
    /* Output:                                                        */
    /* - Array con lo stato ("success" o "error") e un messaggio      */
    /*----------------------------------------------------------------*/
    public function updateLotto($num_prg, $alf_pro, $ubi_qta) {
        try {
            /*--------------------------------------------------------*/
            /* Inizio della transazione per garantire la coerenza dei */
            /* dati                                                   */
            /*--------------------------------------------------------*/
            $this->pdo->beginTransaction();
            
            /*--------------------------------------------------------*/
            /* Ricerca della riga del lotto nella tabella abb_lot     */
            /* utilizzando num_prg e alf_pro come chiavi              */
            /*--------------------------------------------------------*/
            $sqlSelect = "SELECT qta_rig FROM abb_lot WHERE num_prg = :num_prg AND alf_pro = :alf_pro";
            $stmtSelect = $this->pdo->prepare($sqlSelect);
            $stmtSelect->bindParam(':num_prg', $num_prg, PDO::PARAM_STR);
            $stmtSelect->bindParam(':alf_pro', $alf_pro, PDO::PARAM_STR);
            $stmtSelect->execute();
            $row = $stmtSelect->fetch(PDO::FETCH_ASSOC);
            
            if (!$row) {
                /*----------------------------------------------------*/
                /* Se la riga non viene trovata, rollback e ritorno   */
                /* errore                                             */
                /*----------------------------------------------------*/
                $this->pdo->rollBack();
                return array("status" => "error", "message" => "Lotto non trovato per il prodotto " . $alf_pro);
            }
            /*--------------------------------------------------------*/
            /* Quantità presente nel database                         */
            /*--------------------------------------------------------*/
            $qta_db = $row["qta_rig"];
            /*--------------------------------------------------------*/
            /* Confronto tra la quantità inserita e quella in DB      */
            /*--------------------------------------------------------*/
            if ($ubi_qta == $qta_db) 
            {
                /*----------------------------------------------------*/
                /* Se le quantità sono uguali, si chiude la riga:     */
                /* Imposta qta_rig a 0 e flg_lot a '#' per indicare   */
                /* chiusura                                           */
                /*----------------------------------------------------*/
                $sqlUpdate = "UPDATE abb_lot SET flg_lot = '#' 
                              WHERE num_prg = :num_prg AND alf_pro = :alf_pro";
            } 
            else if ($ubi_qta < $qta_db) 
            {
                /*----------------------------------------------------*/
                /* Se la quantità inserita è minore, si sottrae e si  */
                /* aggiorna la riga                                   */
                /*----------------------------------------------------*/
                $nuova_qta = $qta_db - $ubi_qta;
                $sqlUpdate = "UPDATE abb_lot SET qta_rig = :nuova_qta 
                              WHERE num_prg = :num_prg AND alf_pro = :alf_pro";
            } 
            else 
            {
                /*----------------------------------------------------*/
                /* Se la quantità inserita è maggiore della quantità  */
                /* in DB, restituisce un errore                       */
                /*----------------------------------------------------*/
                $this->pdo->rollBack();
                return array("status" => "error", "message" => "Quantità inserita superiore a quella presente");
            }
            /*--------------------------------------------------------*/
            /* Preparazione e esecuzione dell'update                  */
            /*--------------------------------------------------------*/
            $stmtUpdate = $this->pdo->prepare($sqlUpdate)             ;
            $stmtUpdate->bindParam(':num_prg', $num_prg, PDO::PARAM_STR);
            $stmtUpdate->bindParam(':alf_pro', $alf_pro, PDO::PARAM_STR);
            if (isset($nuova_qta)) {
                $stmtUpdate->bindParam(':nuova_qta', $nuova_qta, PDO::PARAM_INT);
            }
            $stmtUpdate->execute()                                    ;
            /*--------------------------------------------------------*/
            /* Conferma della transazione                             */
            /*--------------------------------------------------------*/
            $this->pdo->commit()                                      ;
            return array("status" => "success", "message" => "Lotto aggiornato correttamente");
        } catch (PDOException $e) {
            /*--------------------------------------------------------*/
            /* In caso di errore, annulla la transazione              */
            /*--------------------------------------------------------*/
            $this->pdo->rollBack();
            return array("status" => "error", "message" => "Errore nell'aggiornamento: " . $e->getMessage());
        }
    }

}
?>
