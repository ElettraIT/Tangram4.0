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


# PROBLEMI CON l'ACCOUNT Twilio




use Net::SSH::Perl;

my $ssh = Net::SSH::Perl->new('10.1.10.249');
$ssh->login('nicoladekunovich', 'lascis88');  # Sostituisci con le credenziali dell'utente Mac

my $message = "Ciao, questo è un messaggio dal server!";
my $command = "osascript -e 'tell application \"System Events\" to display dialog \"$message\" buttons {\"OK\"} default button 1'";

my ($stdout, $stderr, $exit) = $ssh->cmd($command);

print "Messaggio inviato: $stdout\n" if $stdout;
print "Errore: $stderr\n" if $stderr;


exit();


