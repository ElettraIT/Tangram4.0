<?php
// Iniziamo con la lettura del file CSV
$file = "ele_svf200.csv" ;

// Apriamo il file CSV
$handle = fopen($file, 'r');

// Leggiamo la prima riga (titolo)
$row = fgetcsv($handle, 1000, ';'); // Prendiamo la prima riga
$titolo = $row[0]; // Accediamo al primo elemento dell'array

// Leggiamo la seconda riga (mesi)
$mesi = fgetcsv($handle, 1000, ';');
array_shift($mesi); // Rimuoviamo il primo valore che non è un mese

// Inizializziamo l'array per i dati
$years_data = array();

// Leggiamo le righe successive (dati)
while ($row = fgetcsv($handle, 1000, ';')) {
    $year_data = array_slice($row, 1); // Rimuoviamo il primo valore che è l'anno
    $year_data_clean = array(); // Array per i dati puliti

    for ($i = 0; $i < count($year_data); $i++) {
        // Rimuoviamo i punti come separatori delle migliaia, ma manteniamo i decimali
        $cleaned_value = str_replace('.', '', $year_data[$i]); // Rimuoviamo solo i punti delle migliaia
        $year_data_clean[] = (float) $cleaned_value; // Conversione a numero decimale
    }
    $years_data[] = $year_data_clean; // Aggiungiamo i dati puliti
}

// Chiudiamo il file
fclose($handle);

// Creiamo manualmente il nostro array di output in formato JSON
$output = '{';
$output .= '"titolo": "' . addslashes($titolo) . '",';
$output .= '"mesi": ["' . implode('","', $mesi) . '"],';
$output .= '"years_data": [';

// Creiamo una stringa per i dati di ogni anno
foreach ($years_data as $key => $year) {
    $output .= '[' . implode(',', $year) . ']';
    if ($key < count($years_data) - 1) {
        $output .= ',';
    }
}

$output .= ']}';

// Impostiamo l'header per restituire un JSON
header('Content-Type: application/json');
echo $output;
?>
