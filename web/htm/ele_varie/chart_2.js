/*========================================*/
/* Controllo dei dati                     */
/*----------------------------------------*/
        $(document).ready(function() {
            $.getJSON('chart_2_csv.php', function(data) {
                // Imposta il titolo del grafico
                $('#grafico').text(data.titolo);

                var ctx = document.getElementById('grafico').getContext('2d');
                
                var chart = new Chart(ctx, {
                    type: 'bar',
                    data: {
                        labels: data.mesi, // Mesi
                        datasets: [
                            {
                                label: '2022',
                                data: data.years_data[0], // Dati per il 2022
                                backgroundColor: '#ff5733', // Colore delle barre
                                borderColor: '#ff5733', // Colore del bordo
                                borderWidth: 1
                            },
                            {
                                label: '2023',
                                data: data.years_data[1], // Dati per il 2023
                                backgroundColor: '#33c9ff',
                                borderColor: '#33c9ff',
                                borderWidth: 1
                            },
                            {
                                label: '2024',
                                data: data.years_data[2], // Dati per il 2024
                                backgroundColor: '#ffeb33',
                                borderColor: '#ffeb33',
                                borderWidth: 1
                            }
                        ]
                    },
                    options: {
                        responsive: true,
                        scales: {
                            y: {
                                beginAtZero: true,
                                ticks: {
                                    callback: function(value) {
                                        return value.toLocaleString(); // Formattiamo i numeri con separatore delle migliaia
                                    }
                                }
                            }
                        },
                        plugins: {
                            tooltip: {
                                callbacks: {
                                    label: function(tooltipItem) {
                                        return tooltipItem.raw.toLocaleString(); // Formattiamo i numeri nel tooltip
                                    }
                                }
                            },
                            datalabels: {
                                anchor: 'end',
                                align: 'top',
                                formatter: function(value) {
                                    return value.toLocaleString(); // Mostra il valore in cima alla barra
                                },
                                color: '#000',
                                font: {
                                    weight: 'bold'
                                }
                            }
                        }
                    }
                });
            });
        });
