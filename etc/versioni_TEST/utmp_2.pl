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

my $ssh = Net::SSH::Perl->new('macos_client_ip');
$ssh->login('username', 'password');  # Sostituisci con le credenziali dell'utente Mac

my $message = "Ciao, questo è un messaggio dal server!";
my $command = "osascript -e 'tell application \"System Events\" to display dialog \"$message\" buttons {\"OK\"} default button 1'";

my ($stdout, $stderr, $exit) = $ssh->cmd($command);

print "Messaggio inviato: $stdout\n" if $stdout;
print "Errore: $stderr\n" if $stderr;


exit();










#==========================================*
# Moduli Perl utilizzati                   *
#------------------------------------------*
use strict                                ;
use warnings                              ;
use LWP::UserAgent                        ;
use HTTP::Request                         ;
use MIME::Base64                          ;
use URI::Escape                           ;




my $message = "Ciao, questo è un messaggio dal server!";
my $user = "nome_utente";   # Nome dell'utente a cui inviare il messaggio

# Costruisci il comando AppleScript
my $applescript = "osascript -e 'tell application \"System Events\" to display dialog \"$message\" buttons {\"OK\"} default button 1'";

# Esegui il comando
system($applescript);


exit();


#==========================================*
# Messaggio via TTY                        *
#------------------------------------------*
my $user = "tangram";   # Nome dell'utente
my $tty  = "pts/6";          # Nome del terminale
my $message = "Messaggio sul terminale specifico! - [F5 per refresh]";

# Esegui il comando write con l'utente e il terminale specificato
system("write $user $tty < /dev/null <<< '$message'");


exit();










#==========================================*
# Credenziali Twilio da account            *
#------------------------------------------*
my $sid = 'ACf86d4a2351c9927b948379d5739358b0';
my $tok = '9a55c356a0fccf93efcae4a26ce9472d';

#==========================================*
# Mittente Twilio da account               *
#------------------------------------------*
# my $mit = 'whatsapp:+14179003836'         ;
my $mit = '+14179003836'         ;

#==========================================*
# ___ DA IMPLEMENTARE DATI IN INPUT ___    *
#------------------------------------------*
#my $dst = shift                           ;
#my $msg = shift                           ;

#==========================================*
# Destinatario                             *
#                                          *
# ___ CON Whatsapp NON VA ___              *
#------------------------------------------*
# my $dst = 'whatsapp:+393493256833‬'        ; # Annalisa
my $dst = '+393493256833‬'        ; # Annalisa
# my $dst = 'whatsapp:+393482229125‬'        ; # Io
# my $dst = '+393482229125‬'        ; # Io
# my $dst = "whatsapp:+393388173833‬‬"      ; # Andrea

#==========================================*
# Test del messaggio                       *
#------------------------------------------*
my $msg = 'Ciao, messaggio inviato da Nicola!';

#==========================================*
# URL per l'API Twilio                     *
#------------------------------------------*
my $url = 'https://api.twilio.com/2010-04-01/Accounts/'.$sid.'/Messages.json';

#==========================================*
# Preparazione dati per la richiesta       *
#------------------------------------------*
my %form = (
    From => $mit,
    To => $dst,
    Body => $msg,
);

#==========================================*
# Pulizia del numero Destinatario          *
#------------------------------------------*
$form{To} =~ s/\s+//g;  # Rimuove tutti gli spazi
$form{To} =~ s/\s*$//g;  # Rimuove gli spazi finali
print "Numero To: $form{To}\n";  # Controlla il valore prima di inviarlo

#==========================================*
# Crea la richiesta HTTP POST              *
#------------------------------------------*
my $uag = LWP::UserAgent->new             ;
my $req = HTTP::Request->new(POST => $url);
$req->authorization_basic($sid, $tok)     ;
$req->content_type('application/x-www-form-urlencoded');
$req->content( join('&', map { "$_=" . uri_escape($form{$_}) } keys %form) );

#==========================================*
# Invia la richiesta                       *
#------------------------------------------*
my $res = $uag->request($req)             ;

#==========================================*
# Verifica la risposta                     *
#------------------------------------------*
if ($res->is_success)
{
    print "Messaggio inviato correttamente!\n";
}
 else
{
    print "Errore nell'invio del messaggio: " . $res->status_line . "\n";
    print "Dettagli dell'errore: " . $res->content . "\n";
}
