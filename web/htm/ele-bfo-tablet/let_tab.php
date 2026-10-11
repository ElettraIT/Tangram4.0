<?php
// let_tab.php
// ==================================================================
// Modulo             : let_tab.php
// Descrizione        : Funzioni per la lettura delle tabelle MySQL
// ==================================================================

// Parametri di connessione al database (modifica questi valori in base al tuo ambiente)
$dbHost = "localhost";      // Indirizzo del server MySQL
$dbName = "ele";  // Nome del database
$dbUser = "root";    // Username del database
$dbPass = "loppio2";       // Password del database

/**
 * Ritorna una connessione PDO al database.
 *
 * @return PDO
 */
function connectDatabase() {
    global $dbHost, $dbName, $dbUser, $dbPass;
    try {
        $dsn = "mysql:host=$dbHost;dbname=$dbName;";
        $pdo = new PDO($dsn, $dbUser, $dbPass, array(
            PDO::ATTR_ERRMODE => PDO::ERRMODE_EXCEPTION,
            PDO::ATTR_DEFAULT_FETCH_MODE => PDO::FETCH_ASSOC
        ));
        return $pdo;
    } catch (PDOException $e) {
        die("Errore di connessione: " . $e->getMessage());
    }
}

/**
 * Recupera la lista degli operatori dalla tabella "operatori".
 *
 * @return array
 */
function getOperatori() {
    $pdo = connectDatabase();
    $sql = "SELECT * FROM zrm";
    $stmt = $pdo->prepare($sql);
    $stmt->execute();
    return $stmt->fetchAll();
}

/**
 * Recupera la lista dei lotti.
 *
 * @return array
 */
function getLotti() {
    $pdo = connectDatabase();
    $sql = "SELECT * FROM abb_lot";
    $stmt = $pdo->prepare($sql);
    $stmt->execute();
    return $stmt->fetchAll();
        }


?>


