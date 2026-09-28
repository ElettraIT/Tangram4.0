#!/usr/bin/perl -w
#==========================================*
# Modulo Perl di prova per inviare messag- *
# gi Whatsapp dal server tramite servizio  *
# gratuito 'Twilio'                        *
#                                          *
# ---------------------------------------- *
# Versione originale:   001 del 15/02/2025 *
#   Ultima revisione:   wip del 17/02/2025 *
# ---------------------------------------- *
#             Autore:   Nicola de Kunovich *
# ---------------------------------------- *
#                                          *
# In input       : - destinatario          *
#                   (cellulare WA)         *
#                                          *
#                  - messaggio             *
#                   (cellulare WA)         *
#                                          *
# (Configurazione: - DA IMPLEMENTARE       *
#                                          *
# (LOG           : - DA IMPLEMENTARE       *
#                                          *
# ---------------------------------------- *
#==========================================*



# FUNZIONA MA NON SERVE A NIENTE ATTUALMENTE ...



use IO::Socket::INET;

# Crea un socket in ascolto sulla porta 12345
my $server = IO::Socket::INET->new(
    LocalPort => 12345,
    Type      => SOCK_STREAM,
    Reuse     => 1,
    Listen    => 5
) or die "Non riesco a creare il socket: $!\n";

print "Server in ascolto sulla porta 12345...\n";

while (my $client = $server->accept()) {
    print $client "Ciao dal server!\n";
    close $client;
}
