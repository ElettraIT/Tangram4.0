/*========================================*/
/* eleinv00 - Javascript - by WIP         */
/*                                        */
/* Elettra : Controllo inventario         */
/* -------------------------------------- */
/* Aggiornamento   : 22/11/2024           */
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
        url: url_get,
        type: 'POST',
        data: dat,
        dataType: 'json',
        success: function(data)
        {
            /*----------------------------*/
            /* Estrazione etichette e va- */
            /* lori                       */
            /*----------------------------*/
            var values = []               ;
            var colors = []               ;
            var labels = []               ;
            data.forEach(function(item)
            {
                values.push(item.value);
                colors.push(item.color);
                labels.push(item.label);
            });
            /*----------------------------*/
            /* Creazione del grafico      */
            /*----------------------------*/
            var ctx = document.getElementById('grf_pie').getContext('2d');
            var grf_pie = new Chart(ctx, {
                /*------------------------*/
                /* Tipo di grafico        */
                /*------------------------*/
                type: 'pie',
                /*------------------------*/
                /* Parametri grafico      */
                /*------------------------*/
                data: {
                    labels: labels,
                    datasets: [{
                        data: values,
//                        backgroundColor: ['orange', 'green'],
                        backgroundColor: colors,
                    }]
                },
                /*------------------------*/
                /* Opzioni grafico        */
                /*------------------------*/
                options: {
                    plugins: {
                        legend: {
                            display: true,
                            labels: {
                            
                                font: {
                                size :16},
                            
                                generateLabels: function (chart) {
                                    return chart.data.labels.
                                        map(function (label, i) {
                                            let backgroundColor = chart.data.
                                                datasets[0].backgroundColor[i];
                                            return {
                                                text: label + ': ' + chart.data.
                                                    datasets[0].data[i],
                                                fillStyle: backgroundColor
                                            };
                                        });
                                }
                            }
                        }
                    }
                }                    
                    
                });
            },
                error: function(xhr, status, error)
                {
                console.error('Errore AJAX:', error);
                }
    });

/*----------------------------------------*/
/* Fine controllo dei dati                */
/*========================================*/
});

