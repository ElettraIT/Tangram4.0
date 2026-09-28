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


____ NON FUNZIONA ___




#==========================================*
# Moduli Perl utilizzati                   *
#------------------------------------------*


use SMS::Send;

#==========================================*
# Credenziali Twilio da account            *
#------------------------------------------*
my $sid = 'ACf86d4a2351c9927b948379d5739358b0';
my $tok = '9a55c356a0fccf93efcae4a26ce9472d';
my $mit = '+14179003836'         ;
my $dst = '+393482229125‬'        ; # Io




# Create an object. There are three required values:
my $sender = SMS::Send->new('Twilio',
  _accountsid => $sid,
  _authtoken  => $tok,
  _from       => $mit,
);
# Send a message to me
my $sent = $sender->send_sms(
  text => 'Messages can be up to 1600 characters',
  to   => $dst,
);
# Did it send?
if ( $sent ) {
  print "Sent test message\n";
} else {
  print "Test message failed\n";
}

