/*========================================*/
/* eleinvh_ - Javascript - by A&N         */
/*                                        */
/* Elettra : Controllo inventario         */
/*                                        */
/* Generazione asincrona grafico a torta  */
/* -------------------------------------- */
/* Aggiornamento   : 20/11/2024           */
/*========================================*/
/*========================================*/
/* Variabili statiche                     */
/*----------------------------------------*/
url_get = '../cgi-bin/dati_pie3.pl'       ;


/*========================================*/
/* Controllo dei dati                     */
/*----------------------------------------*/
$(document).ready(function()
{

    /*====================================*/
    /* Avvia il caricamento e la genera-  */
    /* zione del grafico                  */
    /*------------------------------------*/
    loadPieChart(url_get)                 ;

/*----------------------------------------*/
/* Fine controllo dei dati                */
/*========================================*/
});

/*========================================*/
/* FUNZIONI                               */
/*========================================*/

/*========================================*/
/* Funzione principale per caricare i     */
/* dati e generare il grafico a torta.    */
/*                                        */
/* Usa fetch per ottenere i dati da un    */
/* endpoint server e Chart.js per         */
/* renderizzare il grafico                */
/*----------------------------------------*/
async function loadPieChart(get_url)
{
    /*====================================*/
    /* Inizializzazione                   */
    /*------------------------------------*/
    try
    {
        /*--------------------------------*/
        /* Contatto del server tramite    */
        /* url in input                   */
        /*--------------------------------*/
        const response = await fetch(get_url);
        /*--------------------------------*/
        /* Controllo dello stato della    */
        /* risposta                       */
        /*--------------------------------*/
        if (!response.ok) throw new Error(`Errore HTTP: ${response.status}`);
        /*--------------------------------*/
        /* Parsing della risposta JSON    */
        /*--------------------------------*/
        const data = await response.json();
        /*--------------------------------*/
        /* Estrazione dati JSON - etich.  */
        /*--------------------------------*/
        const labels = data.map(item => item.label);
        /*--------------------------------*/
        /* Estrazione dati JSON - valori  */
        /*--------------------------------*/
        const values = data.map(item => item.value);
        /*--------------------------------*/
        /* Estrazione dati JSON - colori  */
        /*--------------------------------*/
        const colors = data.map(item => item.color);
        /*--------------------------------*/
        /* Calcolo del totale e delle     */
        /* percentuali                    */
        /*--------------------------------*/
        const total = values.reduce((acc, val) => acc + val, 0);
        const percentages = values.map(val => (val / total * 100).toFixed(2));
        /*--------------------------------*/
        /* Selezione del contesto del     */
        /* canvas per il grafico          */
        /*--------------------------------*/
        const ctx = document.getElementById('myPieChart').getContext('2d');
        /*--------------------------------*/
        /* Creazione e configurazione del */
        /* grafico a torta                */
        /*--------------------------------*/
        new Chart(ctx,
        {
            /*----------------------------*/
            /* Grafico a torta            */
            /*----------------------------*/
            type: 'pie',
            /*----------------------------*/
            /* Dati per il grafico        */
            /*----------------------------*/
            data:
            {
                /*------------------------*/
                /* Etichette per ogni     */
                /* fetta                  */
                /*------------------------*/
                labels: labels,
                /*------------------------*/
                /* Set di dati            */
                /*------------------------*/
                datasets: [{
                    /*--------------------*/
                    /* Valori per fetta   */
                    /*--------------------*/
                    data: values,
                    /*--------------------*/
                    /* Colori per fetta   */
                    /*--------------------*/
                    backgroundColor: colors,
                }]
            },
            /*----------------------------*/
            /* Opzioni per il grafico     */
            /*----------------------------*/
            options:
            {
                /*------------------------*/
                /* Responsivo ???         */
                /*------------------------*/
                responsive: true,
                /*------------------------*/
                /* Accessori              */
                /*------------------------*/
                plugins:
                {
                    /*--------------------*/
                    /* Configurazione     */
                    /* della legenda      */
                    /*--------------------*/
                    legend:
                    {
                        /*----------------*/
                        /* Posizionamento */
                        /* a pie' grafico */
                        /*----------------*/
                        position: 'bottom',
                        /*----------------*/
                        /* Etichette      */
                        /*----------------*/
                        labels:
                        {
                            generateLabels: function(chart) {
                                const original = Chart.overrides.pie.plugins.legend.labels.generateLabels;
                                const labels = original(chart);

                                // Aggiungi la percentuale accanto a ogni etichetta
                                labels.forEach(function(label, index) {
                                    label.text += ` (${percentages[index]}%)`;
                                });
                                return labels;
                            }
                        }
                    },
                    /*--------------------*/
                    /* Configurazione     */
                    /* dei tooltip        */
                    /*--------------------*/
                    tooltip:
                    {
                        callbacks:
                        {
                            label: function(context)
                            {
                                const index = context.dataIndex; // Indice della fetta
                                const value = context.raw; // Valore della fetta
                                const percentage = ((value / total) * 100).toFixed(2); // Calcolo della percentuale
                                return `${context.label}: ${value} (${percentage}%)`;
                            }
                        }
                    }
                }
            }
        });
    /*====================================*/
    /* Controllo errori                   */
    /*------------------------------------*/
    } catch (error)
    {
        /*--------------------------------*/
        /* Gestione degli errori durante  */
        /* la richiesta o la generazione  */
        /* del grafico                    */
        /*--------------------------------*/
        console.error("Errore durante il caricamento dei dati: ", error);
        /*--------------------------------*/
        /* Visualizzazione errore         */
        /*--------------------------------*/
        document.body.insertAdjacentHTML
        (
            'beforeend',
            `<p style="color:red;">Errore nel caricamento dei dati. Riprova più tardi.</p>`
        );
    }
/*----------------------------------------*/
/* Fine funzione asincrona                */
/*========================================*/
}

