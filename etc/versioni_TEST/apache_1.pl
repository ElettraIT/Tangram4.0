#!/usr/bin/perl -w
use strict;
use warnings;
use Time::Local;

# Percorso del file di log
my $log_file = '/var/log/apache2/access_log';  # Modifica il percorso del tuo file log
open(my $fh, '<', $log_file) or die "Impossibile aprire '$log_file': $!";

my %active_users;
my $time_window = 300;  # Finestra temporale in secondi (5 minuti)

# Mappa dei mesi (da stringa a numero)
my %months = (
    'Jan' => 0, 'Feb' => 1, 'Mar' => 2, 'Apr' => 3, 'May' => 4, 'Jun' => 5,
    'Jul' => 6, 'Aug' => 7, 'Sep' => 8, 'Oct' => 9, 'Nov' => 10, 'Dec' => 11
);

while (my $line = <$fh>) {
    # Estrai l'IP e il timestamp dalla riga del log
    if ($line =~ /^(\S+) - - \[(.*?)\] ".*?" \d+ \d+ "(.*?)" "(.*?)"/) {
        my $ip = $1;
        my $timestamp = $2;

        # Estrai la data (giorno, mese, anno, ora, minuto, secondo)
        if ($timestamp =~ /(\d{2})\/(\w{3})\/(\d{4}):(\d{2}):(\d{2}):(\d{2})/) {
            my ($day, $month, $year, $hour, $minute, $second) = ($1, $2, $3, $4, $5, $6);

            # Converti il mese da stringa a numero
            my $month_num = $months{$month} // die "Mese non valido: $month";

            # Calcola il timestamp Unix
            my $time_epoch = timelocal($second, $minute, $hour, $day, $month_num, $year - 1900);  # Anno in timelocal è da 1900

            # Rimuovi gli utenti inattivi
            my $current_time = time;
            if ($current_time - $time_epoch <= $time_window) {
                $active_users{$ip} = 1;  # Utente attivo
            }
        }
    }
}

close($fh);

print "Utenti attivi:\n";
foreach my $ip (keys %active_users) {
    print "$ip\n";
}
