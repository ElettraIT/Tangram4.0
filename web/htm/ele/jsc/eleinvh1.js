/*========================================*/
/* eleinvh1 - Javascript - by WIP         */
/*                                        */
/* Elettra : Controllo inventario         */
/*                                        */
/* ___ SOSTITUITO DA 'eleinvh0.js'        */
/* -------------------------------------- */
/* Aggiornamento   : 19/11/2024           */
/*========================================*/
/*========================================*/
/* Variabili statiche                     */
/*----------------------------------------*/
url_get = '../../cgi-bin/ele/eleinvh0'    ;

/*========================================*/
/* Controllo dei dati                     */
/*----------------------------------------*/
$(document).ready(function()
{

    /*====================================*/
    /* Tipo di grafico                    */
    /*------------------------------------*/
    var tip_grf = $('#tip_grf').val()     ;
    /*------------------------------------*/
    /* Oggetto con i Parametri            */
    /*------------------------------------*/
    const prm =
    {
        "tip_grf": tip_grf
    }
    /*------------------------------------*/
    /* Assemblaggio parametri             */
    /*------------------------------------*/
    const dat = $.param(prm)              ;

    /*====================================*/
    /* Caricamento                        */
    /*------------------------------------*/
    $.ajax({
        /*--------------------------------*/
        /* Parametri                      */
        /*--------------------------------*/
        url : url_get,
            type: 'POST',
            dataType: 'json',
            success: function(data) {
                // Estrai etichette, valori e colori dal JSON
                const values = data.map(item => item.value);
                const colors = data.map(item => item.color);
                const labels = data.map(item => item.label);

                // Calcola il totale per calcolare le percentuali
                const total = values.reduce((acc, val) => acc + val, 0);
                const percentages = values.map(val => (val / total * 100).toFixed(2));

                // Crea il grafico a torta
                const ctx = document.getElementById('myPieChart').getContext('2d');
                new Chart(ctx, {
                    type: 'pie', // Tipo di grafico a torta
                    data: {
                        labels: labels, // Etichette per ciascun spicchio
                        datasets: [{
                            data: values, // Valori per ciascun spicchio
                            backgroundColor: colors, // Colori per ciascun spicchio
                        }]
                    },
                    options: {
                        responsive: true,
                        plugins: {
                            legend: {
                                position: 'bottom',
                                labels: {
                                    generateLabels: function(chart) {
                                        const original = Chart.overrides.pie.plugins.legend.labels.generateLabels;
                                        const labels = original(chart);

                                        // Aggiungi la percentuale nella legenda
                                        labels.forEach(function(label, index) {
                                            label.text += ' (' + percentages[index] + '%)';
                                        });
                                        return labels;
                                    }
                                }
                            },
                            // Configurazione dei tooltip
                            tooltip: {
                                callbacks: {
                                    label: function(context) {
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
            },
            error: function(xhr, status, error) {
                console.error("Errore durante il caricamento dei dati: ", error);
            }
        });

/*----------------------------------------*/
/* Fine controllo dei dati                */
/*========================================*/
});

