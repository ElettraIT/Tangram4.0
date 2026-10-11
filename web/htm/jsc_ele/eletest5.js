/*========================================*/
/* eleods01 - Javascript - by WIP         */
/* -------------------------------------- */
/* Aggiornamento   : 07/07/2023           */
/*========================================*/
/*========================================*/
/* Variabili statiche                     */
/*----------------------------------------*/

/*========================================*/
/* Controllo dei dati                     */
/*----------------------------------------*/
$(document).ready(function()
{






    $.fn.dataTable.moment( 'DD/MM/YYYY' );

    /*------------------------------------*/
    /* Personalizzazioni per DataTable    */
    /*------------------------------------*/
    $('#mxport').DataTable({
        /*--------------------------------*/
        /* Definizioni per numero righe   */
        /* per pagina                     */
        /*--------------------------------*/
        dom: 'Bfrtip',
        lengthMenu: [
            [ 10, 25, 50, -1 ],
            [ '10 righe', '25 righe', '50 righe', 'Mostra tutte' ]
        ],
        buttons: [
            'pageLength', 'copy', 'csv', 'excel', 'print'
            
        ],

        /*--------------------------------*/
        /* Definizioni per Italiano       */
        /*--------------------------------*/
        language:
        {
            /*----------------------------*/
            /* Numeri in formato europeo  */
            /*----------------------------*/
            decimal       : ",",
            thousands     : ".",
            /*----------------------------*/
            /* Prompt di elaborazione     */
            /*----------------------------*/
            processing    : "Caricamento ...",
            search        : "Ricerca : ",
            /*----------------------------*/
            /* Prompt risultati           */
            /*----------------------------*/
            lengthMenu    : "Visualizza _MENU_ records per pagina",
            info          : "",
            infoEmpty     : "Nessun record",
            infoFiltered  : "(Filtrato da _MAX_  records)",
            infoPostFix   : "",
            loadingRecords: "Caricamento in corso ...",
            zeroRecords   : "Nessun elemento da mostrare",
            emptyTable    : "Nessun elemento in tabella",
            paginate      : 
            {
                first     : "Prima",
                previous  : "Precedente",
                next      : "Successiva",
                last      : "Ultima"
            },
            /*----------------------------*/
            /* Definizioni per Italiano   */
            /* - bottoni                  */
            /*----------------------------*/
            buttons:
            {
                copy      : "Copia",
                print     : "Stampa",
                pageLength:
                {
                       _: "Mostra %d righe",
                    '-1': "Mostra tutto"
                }
            }
        }
    });







    /*====================================*/
    /* Protocollo ordine                  */
    /*------------------------------------*/
    prt_ods = $('#prt_ods').val()         ;
    /*------------------------------------*/
    /* Responsabile                       */
    /*------------------------------------*/
    rsp_doc = $('#rsp_doc').val()         ;
    rsp_des = $('#rsp_doc option:selected').text();
    /*------------------------------------*/
    /* Focus                              */
    /*------------------------------------*/
    if (prt_ods == "")
    {
        $('#prt_ods').focus()             ;
    }
     else
    {
        $('#alf_pro').focus()             ;
    }

    /*====================================*/
    /* Highlight Numero protocollo        */
    /*------------------------------------*/
    $('#prt_ods').click(function()
    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select()                  ;
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        $("#tes td").css({"background-color": "#BBB"});
        cel_css("cel_num", "+")           ;
    });
    
    /*====================================*/
    /* Highlight Prodotto                 */
    /*------------------------------------*/
    $('#alf_pro').click(function()
    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select()                  ;
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        $("#tes td").css({"background-color": "#BBB"});
        cel_css("cel_pro", "+")           ;
    });

    /*====================================*/
    /* Eventuale switch iniziale campi    */
    /*------------------------------------*/
    if (!rsp_doc || rsp_doc == 0)
    {
        $('#prt_ods').prop("disabled",true);
        $('#alf_pro').prop("disabled",true);
        $('#rsp_doc').focus()             ;
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        $("#tes td").css({"background-color": "#BBB"});
        cel_css("cel_ute", "R")           ;
    }
     else
    {
        $('#prt_ods').prop("enabled",true);
        $('#alf_pro').prop("enabled",true);
        cel_css("cel_col", "+")           ;
        $('#prt_ods').focus()             ;
    }

    /*====================================*/
    /* Cambio Operatore                   */
    /*------------------------------------*/
    $('#rsp_doc').change(function()
    {
        /*--------------------------------*/
        /* Normalizzazioni                */
        /*--------------------------------*/
        $('#prt_ods').val('')             ;
        $('#alf_pro').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#ods_001").submit()            ;
    });          

    /*====================================*/
    /* Cambio Numero documento            */
    /*------------------------------------*/
    $('#prt_ods').change(function()
    {
        /*--------------------------------*/
        /* Normalizzazioni                */
        /*--------------------------------*/
        $('#alf_pro').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#ods_001").submit()            ;
    });          

    /*====================================*/
    /* Cambio Codice prodotto             */
    /*------------------------------------*/
    $('#alf_pro').change(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#ods_001").submit()            ;
    });          

    /*====================================*/
    /* Cambio Prodotto da verificare      */
    /*------------------------------------*/
    $('#cod_pdv').change(function()
    {
        var pdv = $('#cod_pdv option:selected').text();
        $('#alf_pro').val(pdv);
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#ods_001").submit()            ;
    });          

    /*====================================*/
    /* Bottone reset                      */
    /*------------------------------------*/
    $("#res").click(function()
    {
        /*--------------------------------*/
        /* Normalizzazioni                */
        /*--------------------------------*/
        $('#alf_pro').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#ods_001").submit()            ;
    });

    /*====================================*/
    /* Forzatura campi numerici           */
    /* (classe: numbersOnly)              */
    /*------------------------------------*/
    $('.numbersOnly').keyup(function () { 
        this.value = this.value.replace(/[^0-9\.]/g,'');
    });

    /*====================================*/
    /* Controllo valore massimo campi di  */
    /* input numerici                     */
    /*------------------------------------*/
    $('input[type="number"]').on('input', function ()
    {
        var value = $(this).val()         ;
        if ((value !== '') && (value.indexOf('.') === -1))
        {
            $(this).val(Math.max(Math.min(value, 999999), 0));
        }
    });

    /*====================================*/
    /* Accettazione quantita' verificata: */
    /* scorciatoia per copiare quantita'  */
    /* ordinata                           */
    /*------------------------------------*/
    $('#rig td').click(function()
    {
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_ods = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_ods             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var num_col = $(this).parent().children().index($(this));
        /*--------------------------------*/
        /* Campo di spunta                */
        /*--------------------------------*/
        if ($(ide_row).find('input[type="checkbox"]').is(":checked"))
        {
            /*----------------------------*/
            /* Spunta a 'SI'              */
            /*----------------------------*/
            var flg_spn = 'S'             ;
        }
         else
        {
            /*----------------------------*/
            /* Spunta a 'NO'              */
            /*----------------------------*/
            var flg_spn = 'N'             ;
        }
        /*--------------------------------*/
        /* Test che sia la colonna della  */
        /* quantita' ordinata e non ci    */
        /* sia gia' la spunta             */
        /*--------------------------------*/
        if (num_col == 14)
        {
            /*----------------------------*/
            /* Copia della quantita'      */
            /*----------------------------*/
            if (flg_spn == 'N')
            {
                var qta_ods = $(this).closest("tr").find('#qta').val();
                $(this).closest("tr").find('#qtv').val(qta_ods);
            }
        }
    });

    /*====================================*/
    /* Highlight ubicazioni               */
    /*------------------------------------*/
    $('input[type="checkbox"],.numbersOnly').click(function()
    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select()                  ;
        /*--------------------------------*/
        /* Normalizzazione highlight del- */
        /* le righe                       */
        /*--------------------------------*/
        $("#rig td").css({"background-color": "#FFFFFF"});
        /*--------------------------------*/
        /* Highlight cella                */
        /*--------------------------------*/
        var cel = $(this).closest("td").attr('id');
        $(this).closest("tr").find('#' + cel).css({"background-color": "#BBBBBB"});
    });

    /*====================================*/
    /* Modifica box di spunta o quantita''*/
    /*------------------------------------*/
    $('input[type="checkbox"],.numbersOnly').change(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WR"                ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_ods = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_ods             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var prt_ods = $(ide_row).find('#prt').val();
        var prg_ods = $(ide_row).find('#prg').val();
        var qta_ods = $(ide_row).find('#qta').val();
        var qtv_ods = $(ide_row).find('#qtv').val();
        /*--------------------------------*/
        /* Campo di spunta                */
        /*--------------------------------*/
        if ($(ide_row).find('input[type="checkbox"]').is(":checked"))
        {
            /*----------------------------*/
            /* Spunta a 'SI'              */
            /*----------------------------*/
            var flg_spn = 'S'             ;
            /*----------------------------*/
            /* Disabilitazione input      */
            /*----------------------------*/
            $(ide_row).find('#qtv').prop("disabled",true);
        }
         else
        {
            /*----------------------------*/
            /* Spunta a 'NO'              */
            /*----------------------------*/
            var flg_spn = 'N'             ;
            /*----------------------------*/
            /* Abilitazione input         */
            /*----------------------------*/
            $(ide_row).find('#qtv').prop("disabled",false);
        }

        /*--------------------------------*/
        /* Messaggio di quantita' a zero  */
        /*--------------------------------*/
        if (qtv_ods == 0 && flg_spn == 'S')
           {alert("Quantita' a zero!");}  ;

        /*--------------------------------*/
        /* Parametri                      */
        /*--------------------------------*/
        var prm_01 = "rsp_doc=" + rsp_doc ;
        var prm_02 = "tip_ope=" + tip_ope ;
        var prm_03 = "prt_ods=" + prt_ods ;
        var prm_04 = "prg_ods=" + prg_ods ;
        var prm_05 = "pri_ods=" + pri_ods ;
        var prm_06 = "qta_ods=" + qta_ods ;
        var prm_07 = "qtv_ods=" + qtv_ods ;
        var prm_08 = "flg_spn=" + flg_spn ;
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        var prm = prm_01 + "&" +
                  prm_02 + "&" +
                  prm_03 + "&" +
                  prm_04 + "&" +
                  prm_05 + "&" +
                  prm_06 + "&" +
                  prm_07 + "&" +
                  prm_08                  ;
        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./eleodsw0",
            contentType: "application/x-www-form-urlencoded",
            data: prm,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                $('#msg').html(data)      ;
                /*------------------------*/
                /* Ricarica modulo        */
                /*------------------------*/
                $("#ods_001").submit()    ;
            }
        });
    });          

/*----------------------------------------*/
/* Fine controllo dei dati                */
/*========================================*/
});

/*========================================*/
/* FUNZIONI                               */
/*========================================*/

/*========================================*/
/* Funzione generica di switch colore     */
/* cella                                  */
/*----------------------------------------*/
function cel_css (field, swc)
{
    /*------------------------------------*/
    /* Normalizzazione variabile colore   */
    /*------------------------------------*/
    var color = ''                        ;
    /*------------------------------------*/
    /* Colore in funzione dello switch    */
    /*------------------------------------*/
    if (swc == "-")
    {
        color = 'BBBBBB'                  ;
    }
     else if (swc == "+")
    {
        color = '777777'                  ;
    }
     else if (swc == "R")
    {
        color = 'FF0000'                  ;
    }
     else if (swc == "G")
    {
        color = '00FF00'                  ;
    }
     else if (swc == "W")
    {
        color = 'FFFFFF'                  ;
    }
     else
    {
        color = 'BBBBBB'                  ;
    }
    /*------------------------------------*/
    /* Aggiornamento sfondo cella         */
    /*------------------------------------*/
    $("#"+field).css("background-color",
                           "#" + color)   ;
/*----------------------------------------*/
/* Fine funzione                          */
/*========================================*/
}

