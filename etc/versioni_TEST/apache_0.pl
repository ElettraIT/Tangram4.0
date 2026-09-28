#!/usr/bin/perl -w

use strict;
use warnings;

# Percorso del file di log di Apache
my $log_file = '/var/log/apache2/access_log';  # Modifica il percorso in base alla tua configurazione

# Apre il file di log per la lettura
open(my $fh, '<', $log_file) or die "Impossibile aprire il file '$log_file': $!";

# Variabile per tracciare gli IP unici
my %client_ips;

# Leggi il file riga per riga
while (my $line = <$fh>) {
    # Estrai l'IP dalla riga del log (supponiamo il formato di log standard di Apache)
    if ($line =~ /^(\S+)/) {
        my $ip = $1;
        $client_ips{$ip} = 1;  # Aggiungi l'IP alla lista
    }
}

# Stampa gli IP unici
print "Client collegati:\n";
foreach my $ip (keys %client_ips) {
    print "$ip\n";
}

# Chiudi il file
close($fh);
