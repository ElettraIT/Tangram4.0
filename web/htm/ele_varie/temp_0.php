<?php
// Connessione al database usando mysql_connect
$conn = mysql_connect("localhost", "root", "loppio2");
if (!$conn) {
    die('Could not connect: ' . mysql_error());
}

// Seleziona il database
mysql_select_db("test", $conn);

// 1. Genera un identificatore univoco per la tabella temporanea
$unique_id = uniqid('prodotti_', true); // "prodotti_" è il prefisso per il nome della tabella
$table_name = str_replace('.', '_', $unique_id); // Sostituisce il punto con un trattino basso (_)

// 2. Creazione della tabella temporanea con il nome dinamico
$query = "CREATE TEMPORARY TABLE IF NOT EXISTS `$table_name` (
    id INT AUTO_INCREMENT PRIMARY KEY,
    codice_prodotto VARCHAR(50) NOT NULL
)";

if (!mysql_query($query, $conn)) {
    echo "Errore nella creazione della tabella temporanea: " . mysql_error();
    exit;
}

// 3. Inserimento dei dati (codici prodotto)
$prodotti = array('P123', 'P456', 'P789'); // Esempio di codici prodotto

foreach ($prodotti as $codice) {
    $query = "INSERT INTO `$table_name` (codice_prodotto) VALUES ('$codice')";
    if (!mysql_query($query, $conn)) {
        echo "Errore nell'inserimento del prodotto: " . mysql_error();
        exit;
    }
}

// 4. Esegui una SELECT per fare delle operazioni sui dati
$query = "SELECT * FROM `$table_name`";
$result = mysql_query($query, $conn);

        echo "Tabella: " . $table_name . "<br>";


if (mysql_num_rows($result) > 0) {
    while ($row = mysql_fetch_assoc($result))
    {
        echo "Codice prodotto: " . $row['codice_prodotto'] . "<br>";
    }
} else {
    echo "Nessun prodotto trovato.";
}

// 5. Distruggi la tabella temporanea (opzionale)
$query = "DROP TEMPORARY TABLE IF EXISTS `$table_name`";
if (!mysql_query($query, $conn)) {
    echo "Errore nella distruzione della tabella temporanea: " . mysql_error();
}

// Chiudi la connessione
mysql_close($conn);
?>
