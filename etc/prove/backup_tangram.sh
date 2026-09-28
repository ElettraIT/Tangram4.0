#!/bin/bash
#
# Data: 26/03/2013
# Autore: Daniele Calore (daniele.calore @ compu.it)
# Data ultima Modifica: 26/03/2013
#
# Script per il backup giornaliero di Tangram su Filesystem Locale e Remoto (NAS)
# A video l'output, da redirigere su un file di log
#
#############################################

# Directory da salvare
SRC_DIR="/abd"

# Directory di destinazione backup locali
BCK_DIR="/backup_local/tangram"

# FileSystem remoto
BCK_FS_REM="/backup"

# Directory di destinazione backup remoti
BCK_DIR_REM="/backup/tangram"

# Suffisso dei backup
BCK_SUFFIX="bck_tangram"

# Numero di backup da tenere in linea
N_BCK=15

# Impostazioni invio mail
SMTP_SERVER="out1.netclean.it"
SMTP_TO="info@siri-el.com;daniele.dainese@compu.it"
SMTP_FROM="tangram@siri-el.com"
# Notifica solo in caso di errori a Computerland
SMTP_COMPU="notifiche@compu.it;loris.span@compu.it"

#############################################

# Solo root puo' eseguire lo script
ID=$(id -u)
if [ ${ID} -ne 0 ]; then
        echo "# Errore: Solo l'utente root puo' eseguire lo script -- $(date +%Y-%m-%d_%H:%M:%S)"
        exit 64
fi

if [ ! -d ${SRC_DIR} ]; then
        echo "# Errore: La directory da salvare: ${SRC_DIR} non esiste -- $(date +%Y-%m-%d_%H:%M:%S)"
        exit 65
fi

if [ ! -d ${BCK_DIR} ]; then
        echo "# Errore: La directory di backup: ${BCK_DIR} non esiste -- $(date +%Y-%m-%d_%H:%M:%S)"
        exit 66
fi

#############################################

DATA=$(date +%Y-%m-%d_%H-%M)
BCK_NAME="${BCK_SUFFIX}_${DATA}"

echo "##############################################################"
echo "# Inizio backup tangram: ${SRC_DIR} -- $(date +%Y-%m-%d_%H:%M:%S)"
echo "# Salvataggio local backup in: ${BCK_DIR}/${BCK_NAME}.tar.gz"

sync
tar czf ${BCK_DIR}/${BCK_NAME}.tar.gz ${SRC_DIR} >/dev/null 2>&1
RES=$?
echo "# Esito local backup = ${RES}"

# Rotazione backup locali
COUNT=$(ls -1 ${BCK_DIR}/${BCK_SUFFIX}_*tar.gz | wc -l | tr -d ' ')
echo "# Trovati ${COUNT} local backup di tangram (MAX = ${N_BCK})"

while [ ${COUNT} -gt ${N_BCK} ]; do

        # Visto che i local backup sono gia' ordinati per data
        # non serve utilizzare 'ls -1tr' ...
        RM_BCK=$(ls -1 ${BCK_DIR}/${BCK_SUFFIX}_*tar.gz | head -1)
        echo "# Rimozione old local backup: ${RM_BCK}"
        rm -f ${RM_BCK} >/dev/null 2>&1

        COUNT=$(ls -1 ${BCK_DIR}/${BCK_SUFFIX}_*tar.gz | wc -l)
done

# Copia backup locale su share remota
# Verifica che la directory di backup remoto sia su un filesystem diverso da '/'
umount ${BCK_FS_REM} 2>/dev/null
sleep 5
mount ${BCK_FS_REM} 2>/dev/null
sleep 5
BCK_MNT_REM=$(df ${BCK_DIR_REM} 2>/dev/null| tail -1 | awk '{print $NF}')
if [ "Z${BCK_MNT_REM}" = "Z/" ]; then
        echo "# Errore: Remote backup: ${BCK_DIR_REM} e' sul filesystem di root: '/' -- $(date +%Y-%m-%d_%H:%M:%S)"
else
        echo "# Copia local backup su ${BCK_DIR_REM} -- $(date +%Y-%m-%d_%H:%M:%S)"
        cp -p ${BCK_DIR}/${BCK_NAME}.tar.gz ${BCK_DIR_REM}/${BCK_NAME}.tar.gz >/dev/null 2>&1
        RES_REM=$?
        echo "# Fine copia, esito copia backup = ${RES_REM} -- $(date +%Y-%m-%d_%H:%M:%S)"

        # Rotazione backup remoti
        COUNT=$(ls -1 ${BCK_DIR_REM}/${BCK_SUFFIX}_*tar.gz | wc -l | tr -d ' ')
        echo "# Trovati ${COUNT} remote backup di tangram (MAX = ${N_BCK})"

        while [ ${COUNT} -gt ${N_BCK} ]; do

                # Visto che i remote backup sono gia' ordinati per data
                # non serve utilizzare 'ls -1tr' ...
                RM_BCK=$(ls -1 ${BCK_DIR_REM}/${BCK_SUFFIX}_*tar.gz | head -1)
                echo "# Rimozione old remote backup: ${RM_BCK}"
                rm -f ${RM_BCK} >/dev/null 2>&1

                COUNT=$(ls -1 ${BCK_DIR_REM}/${BCK_SUFFIX}_*tar.gz | wc -l)
        done
fi

echo "# Fine backup tangram: ${SRC_DIR} -- $(date +%Y-%m-%d_%H:%M:%S)"
echo "##############################################################"

# Invio Mail:
DATA=$(date +"%F %R:%S")
if [ ${RES} -gt 0 -o ${RES_REM} -gt 0 ]; then
	BODY="Errore backup Tangram su NAS Synology. Verificare file di log: /usr/local/bin/backup_tangram.log"
	SUBJECT="SIRI: ERROR backup Tangram -- ${DATA}"
# Mail 
smtp=${SMTP_SERVER} mailx -s "${SUBJECT}" -r ${SMTP_FROM} ${SMTP_TO} <<__EOF
${BODY}
__EOF
smtp=${SMTP_SERVER} mailx -s "${SUBJECT}" -r ${SMTP_FROM} ${SMTP_COMPU} <<__EOF
${BODY}
__EOF

else
	BODY="OK backup giornaliero Tangram su NAS Synology."
	SUBJECT="SIRI: OK backup Tangram -- ${DATA}"
# Mail 
smtp=${SMTP_SERVER} mailx -s "${SUBJECT}" -r ${SMTP_FROM} ${SMTP_TO} <<__EOF
${BODY}
__EOF
fi

exit ${RES}
#EOF
