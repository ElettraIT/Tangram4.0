/*========================================*/
/* elepro01 - Javascript - by WIP         */
/* -------------------------------------- */
/* Aggiornamento   : 21/10/2024           */
/*========================================*/

/*========================================*/
/* Variabili statiche                     */
/*----------------------------------------*/
mod_frm = "#pro_001"                      ;

/*========================================*/
/* Controllo dei dati                     */
/*----------------------------------------*/
$(document).ready(function()
{
    /*====================================*/
    /* Campi form                         */
    /*------------------------------------*/
    rsp_doc = $('#rsp_doc').val()         ;
    num_pro = $('#num_pro').val()         ;
    alf_pro = $('#alf_pro').val()         ;

    /*====================================*/
    /* Bottone find                       */
    /*------------------------------------*/
    $("#but_fnd").click(function()
    {
        /*--------------------------------*/
        /* Normalizzazione prodotto       */
        /*--------------------------------*/
        $("#alf_pro").val('')             ;
        /*--------------------------------*/
        /* Cambio sfondo prodotto         */
        /*--------------------------------*/
        $("#alf_pro").css("background-color",
                          "#9FE2BF")      ;
        /*--------------------------------*/
        /* Chiusura provvisoria tabelle   */
        /*--------------------------------*/
        $("#gia, #ubi, #pro").hide()      ;
        /*--------------------------------*/
        /* Focus su prodotto              */
        /*--------------------------------*/
        $("#alf_pro").focus()             ;
        /*--------------------------------*/
        /* Attivazione Autocomplete       */
        /*--------------------------------*/
        $("#alf_pro").autocomplete(
        {
            /*----------------------------*/
            /* Esecuzione Autocomplete    */
            /*----------------------------*/
            source     : "./eletest3.pl",
            dataType   : 'json',
            minLength  : 4,
            sortResults: true,
            /*----------------------------*/
            /* Selezione Autocomplete     */
            /*----------------------------*/
            select     : function(event, ui)
            {
                event.preventDefault()    ;
                $("#alf_pro").val(ui.item.value);
                $("#pro_001").submit()    ;
            },
        });
    });

    /*====================================*/
    /* Definizione box di dialogo per     */
    /* espansione giacenze per ubicazione */
    /*------------------------------------*/
    $("#dialog").dialog({
        autoOpen: false,
        modal   : true,
        title   : 'Scheda prodotto',
        width   : 700,
        height  : "auto"
    });

    /*====================================*/
    /* Bottone reset                      */
    /*------------------------------------*/
    $("#but_res").click(function()
    {
        /*--------------------------------*/
        /* Normalizzazioni                */
        /*--------------------------------*/
        $('#alf_pro').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });

    /*====================================*/
    /* Bottone refresh                    */
    /*------------------------------------*/
    $('#but_ref').click(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });
    
    /*====================================*/
    /* Codice prodotto                    */
    /*------------------------------------*/
    cel_css("cel_pro", "+")               ;
    $('#alf_pro').focus()                 ;
    $('#alf_pro').select()                ;

    /*====================================*/
    /* Test su presenza responsabile      */
    /*------------------------------------*/
    if (!rsp_doc)
    {
        /*--------------------------------*/
        /* Test iniziale su indirizzo IP  */
        /* individuare l'operatore        */
        /*--------------------------------*/
        $.ajax({
            type: "POST",
            url : "./getip.pl",
            contentType: "application/x-www-form-urlencoded",
            /*----------------------------*/
            /* Se indirizzo determinato   */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* Determinazione         */
                /*------------------------*/
                var ipx_val = data.trim() ;
                let_ipx(ipx_val)          ;
            }
        });
    };

    /*====================================*/
    /* Selezionato prodotto               */
    /*------------------------------------*/
    $('#alf_pro').click(function()
    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select()                  ;
    });          

    /*====================================*/
    /* Cambio prodotto                    */
    /*------------------------------------*/
    $('#alf_pro').change(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });          

    /*====================================*/
    /* Espansione giacenze ubicazione     */
    /*------------------------------------*/
    $('#ubi td').click(function()
    {
        /*================================*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var num_cln = $(this).parent().children().index($(this));
        var alf_pro = $('#alf_pro').val();
        var cod_ubi = $(this).closest("tr").find('#ubi').val();
        
        /*================================*/
        /* Se colonna dell'ubicazione     */
        /*--------------------------------*/
        if (num_cln == 1)
        {
            /*----------------------------*/
            /* Parametri                  */
            /*----------------------------*/
            var alf_pro = $('#alf_pro').val();
            var num_pro = $('#num_pro').val();
            var ubi_prh = $('#ubi_prh').val();
            var rsp_des = $('#rsp_des').val();
            /*----------------------------*/
            /* Elemento selezionato       */
            /*----------------------------*/
            var ubi_pri = $(this).text().trim();
            /*----------------------------*/
            /* Ubicazioni di transito     */
            /*----------------------------*/
            if ([ubi_prh,
                 'LSPED',
                 'BSPED', 
                  'LRIC'].includes(ubi_pri))
            {return;}
            /*----------------------------*/
            /* Tipo operazione            */
            /*----------------------------*/
            var tip_ope = "UP"            ;
            /*----------------------------*/
            /* Conferma                   */
            /*                            */
            /* Funzione di 'eleubiJ0'     */
            /*----------------------------*/
            dlg_ubi(tip_ope,
                    ubi_pri,
                    num_pro,
                    rsp_des)              ;
        }
        
        /*================================*/
        /* Se colonna della quantita' per */
        /* ubicazione                     */
        /*--------------------------------*/
        if (num_cln == 2)
        {
            /*----------------------------*/
            /* Oggetto con i Parametri    */
            /*----------------------------*/
            const prm =
            {
                "cod_ubi": cod_ubi,
                "alf_pro": alf_pro
            }
            /*----------------------------*/
            /* Assemblaggio parametri     */
            /*----------------------------*/
            const dat = $.param(prm)      ;
            /*----------------------------*/
            /* Riempimento box            */
            /*----------------------------*/
            $.ajax({
                type: "POST",
                url: "./eleprogu",
                contentType: "application/x-www-form-urlencoded",
                data: dat,
                success: function (data)
                {
                    $("#dialog").html(data);
                    $("#dialog").dialog("option","title","Ubicazione : " + cod_ubi).dialog('open');
                }
            });
        }

    });

    /*====================================*/
    /* Espansione quantita' in ordine di  */
    /* spedizione                         */
    /*------------------------------------*/
    $('#qta_ods').click(function()
    {
        /*--------------------------------*/
        /* Test su quantita'              */
        /*--------------------------------*/
        var qta_ods = $('#qta_ods').text().trim() ;
        if (qta_ods == 0){return}         ;
        /*--------------------------------*/
        /* Codice prodotto                */
        /*--------------------------------*/
        var num_pro = $('#num_pro').val() ;
        var alf_pro = $('#alf_pro').val() ;
        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm =
        {
            "num_pro": num_pro
        }
        /*--------------------------------*/
        /* Titolo Dialog                  */
        /*--------------------------------*/
        var dlg_tit = alf_pro
                    + " - In ordine di spedizione";
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm)          ;
        /*--------------------------------*/
        /* Riempimento box                */
        /*--------------------------------*/
        $.ajax({
            type: "POST",
            url: "./eleproos",
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            success: function (data)
            {
                $("#dialog").html(data);
                $("#dialog").dialog("option","title",dlg_tit).dialog('open');
            }
        });
    });
    
    /*====================================*/
    /* Espansione quantita' in ordine a   */
    /* fornitori                          */
    /*------------------------------------*/
    $('#qta_orf').click(function()
    {
        /*--------------------------------*/
        /* Test su quantita'              */
        /*--------------------------------*/
        var qta_orf = $('#qta_orf').text().trim() ;
        if (qta_orf == 0){return}         ;
        /*--------------------------------*/
        /* Codice prodotto                */
        /*--------------------------------*/
        var num_pro = $('#num_pro').val() ;
        var alf_pro = $('#alf_pro').val() ;
        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm =
        {
            "num_pro": num_pro
        }
        /*--------------------------------*/
        /* Titolo Dialog                  */
        /*--------------------------------*/
        var dlg_tit = alf_pro
                    + " - In ordine a fornitori";
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm)          ;
        /*--------------------------------*/
        /* Riempimento box                */
        /*--------------------------------*/
        $.ajax({
            type: "POST",
            url: "./eleproof",
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            success: function (data)
            {
                $("#dialog").html(data);
                $("#dialog").dialog("option","title",dlg_tit).dialog('open');
            }
        });
    });
    
    /*====================================*/
    /* Espansione quantita' in ordine da  */
    /* clienti                            */
    /*------------------------------------*/
    $('#qta_orc').click(function()
    {
        /*--------------------------------*/
        /* Test su quantita'              */
        /*--------------------------------*/
        var qta_orc = $('#qta_orc').text().trim() ;
        if (qta_orc == 0){return}         ;
        /*--------------------------------*/
        /* Codice prodotto                */
        /*--------------------------------*/
        var num_pro = $('#num_pro').val() ;
        var alf_pro = $('#alf_pro').val() ;
        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm =
        {
            "num_pro": num_pro
        }
        /*--------------------------------*/
        /* Titolo Dialog                  */
        /*--------------------------------*/
        var dlg_tit = alf_pro
                    + " - In ordine da clienti";
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm)          ;
        /*--------------------------------*/
        /* Riempimento box                */
        /*--------------------------------*/
        $.ajax({
            type: "POST",
            url: "./eleprooc",
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            success: function (data)
            {
                $("#dialog").html(data);
                $("#dialog").dialog("option","title",dlg_tit).dialog('open');
            }
        });
    });
    
    /*====================================*/
    /* Espansione giacenza presso terzi   */
    /*------------------------------------*/
    $('#qta_ptz').click(function()
    {
        /*--------------------------------*/
        /* Test su quantita'              */
        /*--------------------------------*/
        var qta_ptz = $('#qta_ptz').text().trim() ;
        if (qta_ptz == 0){return}         ;
        /*--------------------------------*/
        /* Codice prodotto                */
        /*--------------------------------*/
        var num_pro = $('#num_pro').val() ;
        var alf_pro = $('#alf_pro').val() ;
        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm =
        {
            "num_pro": num_pro
        }
        /*--------------------------------*/
        /* Titolo Dialog                  */
        /*--------------------------------*/
        var dlg_tit = alf_pro
                    + " - Presso terzi"   ;
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm)          ;
        /*--------------------------------*/
        /* Riempimento box                */
        /*--------------------------------*/
        $.ajax({
            type: "POST",
            url: "./eleprogz",
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            success: function (data)
            {
                $("#dialog").html(data);
                $("#dialog").dialog("option","title",dlg_tit).dialog('open');
            }
        });
    });
    
    /*====================================*/
    /* Bottone Trash riga                 */
    /*------------------------------------*/
    $('.del_btn').click(function()
    {
        /*--------------------------------*/
        /* Parametri                      */
        /*--------------------------------*/
        var alf_pro = $('#alf_pro').val() ;
        var num_pro = $('#num_pro').val() ;
        var rsp_des = ''                  ;
        var ubi_pri = $(this).closest("tr").find("td:first").text().trim();
        /*--------------------------------*/
        /* Se ubicazione a spazi: uscita  */
        /*--------------------------------*/
        if (!ubi_pri || ubi_pri == '')
        {return};
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "DG"                ;
        /*--------------------------------*/
        /* Conferma                       */
        /*                                */
        /* Funzione di 'eleubiJ0'         */
        /*--------------------------------*/
        dlg_ubi(tip_ope,
                ubi_pri,
                num_pro,
                rsp_des)                  ;
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
