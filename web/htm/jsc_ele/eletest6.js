/*========================================*/
/* elebfoB1 - Javascript - N & A          */
/* -------------------------------------- */
/* Aggiornamento   : 26/02/2024           */
/* -------------------------------------- */
/* VERSIONE NUOVA                         */
/*========================================*/
/*========================================*/
/* Variabili statiche                     */
/*----------------------------------------*/

/*========================================*/
/* Controllo dei dati                     */
/*----------------------------------------*/
$(document).ready(function()
{
    /*====================================*/
    /* Variabili globali                  */
    /*------------------------------------*/
    /* Livello operativo                  */
    /*                                    */
    /* - '0' : Accettazione responsabile  */
    /* - '1' : Accettazione protocolli    */
    /* - '2' : Accettazione prodotti      */
    /*------------------------------------*/
    liv_ope = $('#liv_ope').val()         ;

    /*====================================*/
    /* Deviazione in funzione del livello */
    /*------------------------------------*/
    if (liv_ope == 0 || !liv_ope)
    {
        /*--------------------------------*/
        /* Livello '0'                    */
        /*--------------------------------*/
        liv_ope = 0                       ;
        liv_000 ()                        ;
    }
     else if (liv_ope == 1)
    {
        /*--------------------------------*/
        /* Livello '1'                    */
        /*--------------------------------*/
        liv_001 ()                       ;
    }
     else
    {
        /*--------------------------------*/
        /* Livello '2'                    */
        /*--------------------------------*/
        liv_002 ()                       ;
    }

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

/*========================================*/
/* Funzione di gestione livello '0'       */
/*                                        */
/* Input Operatore                        */
/*----------------------------------------*/
function liv_000 ()
{
    /*====================================*/
    /* Cella di accettazione in rosso     */
    /*                                    */
    /* Livello '0'                        */
    /*------------------------------------*/
    cel_css("cel_ute", "R")               ;

    /*====================================*/
    /* Cambio Operatore                   */
    /*                                    */
    /* Livello '0'                        */
    /*------------------------------------*/
    $('#rsp_doc').change(function()
    {
        /*--------------------------------*/
        /* Se impostato                   */
        /*--------------------------------*/
        if ($(this) != '')
        {
            /*----------------------------*/
            /* A livello successivo       */
            /*----------------------------*/
            $('#liv_ope').val('1')        ;
        }
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#bfo_001").submit()            ;
    });          

/*----------------------------------------*/
/* Fine funzione livello '0'              */
/*========================================*/
}

/*========================================*/
/* Funzione di gestione livello '1'       */
/*                                        */
/* Input Protocolli                       */
/*----------------------------------------*/
function liv_001 ()
{
    /*====================================*/
    /* Definizione box di dialogo per     */
    /* espansione elenco 'check'          */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $("#dialog").dialog({
        autoOpen: false,
        modal: true,
        title: 'Scheda prodotto',
        width: 950,
        height: "auto",
    });

    /*====================================*/
    /* Definizione box di dialogo per     */
    /* fotocamera                         */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $("#dialog_img").dialog({
        autoOpen: false,
        modal: true,
        title: 'Fotocamera',
        width: 950,
        height: "auto",
    });

    /*====================================*/
    /* Numero documento                   */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    num_doc = $('#num_doc').val()         ;
    if (num_doc == '')
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
        $('#num_doc').focus()             ;
    }
    
    /*====================================*/
    /* Responsabile                       */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    rsp_doc = $('#rsp_doc').val()         ;
    rsp_des = $('#rsp_doc option:selected').text();
    /*------------------------------------*/
    /* Flag di stampa                     */
    /*------------------------------------*/
    flg_stp = $('#flg_stp').val()         ;

    /*====================================*/
    /* Highlight Numero documento         */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#num_doc').click(function()
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
    /* Cambio Numero documento            */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#num_doc').change(function()
    {
        /*--------------------------------*/
        /* Normalizzazioni                */
        /*--------------------------------*/
        $('#alf_pro').val('')             ;
        $('#prt_bfo').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#bfo_001").submit()            ;
    });          

    /*====================================*/
    /* Highlight Codice prodotto          */
    /*                                    */
    /* Livello '1'                        */
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
    /* Bottone reset                      */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $("#res").click(function()
    {
        /*--------------------------------*/
        /* Normalizzazioni                */
        /*--------------------------------*/
        $('#num_doc').val('')             ;
        $('#prt_bfo').val('')             ;
        $('#alf_pro').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#bfo_001").submit()            ;
    });

    /*====================================*/
    /* Bottone reset codice prodotto      */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $("#rep").click(function()
    {
        /*--------------------------------*/
        /* Se prodotto inserito           */
        /*--------------------------------*/
        var alf = $('#alf_pro').val()     ;
        if (alf != '')
        {
            /*----------------------------*/
            /* Normalizzazione livello    */
            /* operativo                  */
            /*----------------------------*/
            $('#liv_ope').val('')         ;
            /*----------------------------*/
            /* Normalizzazione prodotto   */
            /*----------------------------*/
            $('#alf_pro').val('')         ;
            /*----------------------------*/
            /* Caricamento del modulo     */
            /*----------------------------*/
            $("#bfo_001").submit()        ;
        }
    });

    /*====================================*/
    /* Inibizione totale se stampante     */
    /* verde                              */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    if (flg_stp != '#')
    {
        $('input[type=checkbox]').attr('disabled','true');
    }

    /*====================================*/
    /* Eventuale switch bottone stampa    */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    if (flg_stp == "#" || flg_stp == undefined)
    {
        cel_css("stp", "R")               ;
        $('#stp').prop("disabled",true)   ;
    }
     else
    {
        cel_css("stp", "G")               ;
        $('#stp').prop("enabled",true)    ;
    }
    
    /*====================================*/
    /* Eventuale inibizione codice        */
    /* prodotto                           */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    if (!num_doc || num_doc == "")
    {
        $('#alf_pro').prop("disabled",true);
    }
     else
    {
        $('#alf_pro').prop("enabled",true);
    }
    
    /*====================================*/
    /* Focus su prodotto                  */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#alf_pro').focus()                 ;
    $('#alf_pro').select()                ;

    /*====================================*/
    /* Cambio prodotto                    */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#alf_pro').change(function()
    {
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var prt_bfo = $("#prt_bfo").val() ;
        var alf_pro = $('#alf_pro').val() ;
        
        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm =
        {
            "prt_bfo": prt_bfo,
            "alf_pro": alf_pro
        }
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm)          ;
        /*--------------------------------*/
        /* Lettura                        */
        /*--------------------------------*/
        $.ajax({ type: 'POST',
                  url: "./elebfop0",
          contentType: "application/x-www-form-urlencoded",
                 data: dat,
              success: function(data)
              {
                /*------------------------*/
                /* Esito lettura          */
                /*------------------------*/
                let res = data.substring(0, 2);
                /*------------------------*/
                /* Se test superato       */
                /*------------------------*/
                if (res == 'OK')
                {
                    /*--------------------*/
                    /* Livello successivo */
                    /*--------------------*/
                    $('#liv_ope').val('2');
                }
                /*------------------------*/
                /* Caricamento del modulo */
                /*------------------------*/
                $("#bfo_001").submit()    ;
            } 
        });    
    });          

    /*====================================*/
    /* Gestione click nella riga          */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#rig td').click(function()
    {
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_bfo             ;
        /*--------------------------------*/
        /* Numero colonna                 */
        /*--------------------------------*/
        var num_cln = $(this).parent().children().index($(this));
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var alf_pro = $(ide_row).find('#alf').val();
        var prt_bfo = $(ide_row).find('#prt').val();
        var prg_bfo = $(ide_row).find('#prg').val();
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

        /*================================*/
        /* Se colonna del codice prodotto */
        /*--------------------------------*/
        if (num_cln == 15)
        {
            /*----------------------------*/
            /* A livello successivo solo  */
            /* se NON spuntata            */
            /*----------------------------*/
            if (flg_spn == 'N')
            {
                /*------------------------*/
                /* Progressivo riga       */
                /*------------------------*/
                $('#prg_bfo').val(prg_bfo);
                /*------------------------*/
                /* Codice prodotto        */
                /*------------------------*/
                $('#alf_pro').val(alf_pro);
                /*------------------------*/
                /* A livello successivo   */
                /*------------------------*/
                $('#liv_ope').val('2')    ;
                /*------------------------*/
                /* Ricarica modulo        */
                /*------------------------*/
                $("#bfo_001").submit()    ;
            }
        }

        /*================================*/
        /* Se colonna della descrizione:  */
        /* espansione su colli            */
        /*--------------------------------*/
        if (num_cln == 16)
        {
            /*----------------------------*/
            /* Riempimento box            */
            /*----------------------------*/
            $.ajax({
                type: "POST",
                url: "./elebfoc0",
                contentType: "application/x-www-form-urlencoded",
                data: "prt_bfo="  + prt_bfo +
                      "&prg_bfo=" + prg_bfo,
                success: function (data)
                {
                      $("#dialog").html(data);
                      $("#dialog").dialog("option","title","Colli della riga").dialog('open');
                }
            });
        }

        /*================================*/
        /* Se colonna della quantita'     */
        /* ordinata e non ci sia gia' la  */
        /* spunta                         */
        /*--------------------------------*/
        if (num_cln == 18)
        {
            /*----------------------------*/
            /* Copia della quantita'      */
            /*----------------------------*/
            if (flg_spn == 'N')
            {
                var qta_bfo = $(this).closest("tr").find('#qta').val();
                $(this).closest("tr").find('#qtv').val(qta_bfo);
            }
        }

        /*================================*/
        /* Se colonna delle ubicazioni:   */
        /* espansione su ubicazioni       */
        /*--------------------------------*/
        if (num_cln == 21)
        {
            /*----------------------------*/
            /* Codice prodotto            */
            /*----------------------------*/
            var alf_pro = $(this).closest("tr").find('#alf').val();
            /*----------------------------*/
            /* Riempimento box            */
            /*----------------------------*/
            $.ajax({
                type: "POST",
                url: "./eleodsg0",
                contentType: "application/x-www-form-urlencoded",
                data: "alf_pro=" + alf_pro,
                success: function (data)
                {
                      $("#dialog").html(data);
                      $("#dialog").dialog("option","title","Prodotto : " + alf_pro).dialog('open');
                }
            });
        }

    /*------------------------------------*/
    /* Gestione click nella riga          */
    /*                                    */
    /* Livello '1'                        */
    /*                                    */
    /* Fine                               */
    /*====================================*/
    });

    /*====================================*/
    /* Highlight prodotto                 */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('.numbersOnly').click(function()
    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select()                  ;
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        cel_css("cel_num", "+")           ;
    });

    /*====================================*/
    /* Rilevazione tasto funzione Tangram */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('.numbersOnly').keyup(function(e)
    {
        /*--------------------------------*/
        /* Rilevazione tasto              */
        /*--------------------------------*/
        var tgk = (e.keyCode ? 
	            e.keyCode : 
	            e.which)               ;
        if(tgk == 13){$("#bfo_001").submit()};
    });           

    /*====================================*/
    /* Modifica box di spunta             */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('input[type="checkbox"]').change(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WC"                ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_bfo             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var prt_bfo = $(ide_row).find('#prt').val();
        var prg_bfo = $(ide_row).find('#prg').val();
        var qta_bfo = $(ide_row).find('#qta').val();
        var qtv_bfo = $(ide_row).find('#qtv').val();
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
            $(ide_row).find('#qta').prop("disabled",true);
            $(ide_row).find('#ann').prop("disabled",true);
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
            $(ide_row).find('#qta').prop("disabled",false);
            $(ide_row).find('#ann').prop("disabled",false);
        }

        /*--------------------------------*/
        /* Messaggio di quantita' a zero  */
        /*--------------------------------*/
        if (qtv_bfo == 0 && flg_spn == 'S')
           {alert("Quantita' a zero!");}  ;

        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm =
        {
            "rsp_doc": rsp_doc,
            "tip_ope": tip_ope,
            "prt_bfo": prt_bfo,
            "prg_bfo": prg_bfo,
            "pri_bfo": pri_bfo,
            "qta_bfo": qta_bfo,
            "qtv_bfo": qtv_bfo,
            "flg_spn": flg_spn
        }
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm)          ;

        /*--------------------------------*/
        /* Messaggio per la richiesta di  */
        /* conferma                       */
        /*--------------------------------*/
        var cnf_msg = ''                  ;
        if (flg_spn == 'N')
        {
            cnf_msg = 'Vuoi togliere la spunta al prodotto?'
        }
         else
        {
            cnf_msg = 'Vuoi spuntare manualmente il prodotto?'
        }

        /*================================*/
        /* Richiesta di conferma spunta   */
        /*--------------------------------*/
        cnf_dlg(cnf_msg).done(function()
        {
        /*================================*/
        /* Scelta 'SI'                    */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./elebfox0",
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* Ricarica modulo        */
                /*------------------------*/
                $("#bfo_001").submit()    ;
            }
        });
        /*================================*/
        /* Scelta 'NO'                    */
        /*--------------------------------*/
        }).fail(function()
        {
            /*----------------------------*/
            /* Annullamento spunta        */
            /*----------------------------*/
            $(this).prop('checked',false);
            /*----------------------------*/
            /* Ricarica modulo            */
            /*----------------------------*/
            $("#bfo_001").submit()        ;
        });
    });          

    /*====================================*/
    /* Modifica quantita'                 */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('.numbersOnly').change(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WC"                ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_bfo             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var prt_bfo = $(ide_row).find('#prt').val();
        var prg_bfo = $(ide_row).find('#prg').val();
        var qta_bfo = $(ide_row).find('#qta').val();
        var qtv_bfo = $(ide_row).find('#qtv').val();
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
            $(ide_row).find('#qta').prop("disabled",true);
            $(ide_row).find('#ann').prop("disabled",true);
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
            $(ide_row).find('#qta').prop("disabled",false);
            $(ide_row).find('#ann').prop("disabled",false);
        }

        /*--------------------------------*/
        /* Messaggio di quantita' a zero  */
        /*--------------------------------*/
        if (qtv_bfo == 0 && flg_spn == 'S')
           {alert("Quantita' a zero!");}  ;

        /*--------------------------------*/
        /* Messaggio di quantita' su-     */
        /* periore a quella in ordine     */
        /*--------------------------------*/
        if (qtv_bfo > qta_bfo)
        {
            /*----------------------------*/
            /* Normalizzazione            */
            /*----------------------------*/
            $(ide_row).find('#qtv').val(0);
            qtv_bfo = 0                   ;
            /*----------------------------*/
            /* Messaggio                  */
            /*----------------------------*/
            alert("Quantita' non accettabile!");
            /*----------------------------*/
            /* Uscita                     */
            /*----------------------------*/
            return                        ;
        }

        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm =
        {
            "rsp_doc": rsp_doc,
            "tip_ope": tip_ope,
            "prt_bfo": prt_bfo,
            "prg_bfo": prg_bfo,
            "pri_bfo": pri_bfo,
            "qta_bfo": qta_bfo,
            "qtv_bfo": qtv_bfo,
            "flg_spn": flg_spn
        }
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm)          ;

        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./elebfox0",
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                $('#msg').html(data)      ;
                /*------------------------*/
                /* Ricarica modulo        */
                /*------------------------*/
                $("#bfo_001").submit()    ;
            }
        });
    });          

    /*====================================*/
    /* Bottone Non conformita'            */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('.ncf').click(function()
    {
        /*--------------------------------*/
        /* Identificatori riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var ide_rw2 = "#rw2_" +
                      pri_bfo             ;
        /*--------------------------------*/
        /* Apertura / chiusura riga       */
        /*--------------------------------*/
        $(ide_rw2).toggle()               ;
        /*--------------------------------*/
        /* Normalizzazione highlight del- */
        /* le righe                       */
        /*--------------------------------*/
        $("#rig td").css({"background-color": "#FFFFFF"});
        /*--------------------------------*/
        /* Highlight cella                */
        /*--------------------------------*/
        $(this).closest("tr").find('#cel_ncf').css({"background-color": "#BBBBBB"});
        /*--------------------------------*/
        /* Identificatori riga tabella    */
        /*--------------------------------*/
        var ide_rw4 = "#rw4_" +
                      pri_bfo             ;
        /*--------------------------------*/
        /* Apertura / chiusura riga       */
        /*--------------------------------*/
        $(ide_rw4).toggle()               ;
        /*--------------------------------*/
        /* Normalizzazione highlight del- */
        /* le righe                       */
        /*--------------------------------*/
        $("#rig td").css({"background-color": "#FFFFFF"});
        /*--------------------------------*/
        /* Highlight cella                */
        /*--------------------------------*/
        $(this).closest("tr").find('#cel_ncf').css({"background-color": "#BBBBBB"});
    });

    /*====================================*/
    /* Accettazione testo Non conformita' */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('.inp_ncf').change(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WN"                ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var ide_row = "#rw2_" +
                      pri_bfo             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var prt_bfo = $(ide_row).find('#prt').val();
        var prg_bfo = $(ide_row).find('#prg').val();
        var pri_bfo = $(ide_row).find('#pri').val();
        var ann_ncf = $(ide_row).find('#nct').val();
        /*--------------------------------*/
        /* Parametri                      */
        /*--------------------------------*/
        var prm_01 = "rsp_des=" + rsp_des ;
        var prm_02 = "tip_ope=" + tip_ope ;
        var prm_03 = "prt_bfo=" + prt_bfo ;
        var prm_04 = "prg_bfo=" + prg_bfo ;
        var prm_05 = "pri_bfo=" + pri_bfo ;
        var prm_06 = "ann_ncf=" + ann_ncf ;
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        var prm = prm_01 + "&" +
                  prm_02 + "&" +
                  prm_03 + "&" +
                  prm_04 + "&" +
                  prm_05 + "&" +
                  prm_06                  ;
        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./elebfox0",
            contentType: "application/x-www-form-urlencoded",
            data: prm,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* Eventuale messaggio    */
                /*------------------------*/
                $('#msg').html(data)      ;
                /*------------------------*/
                /* Caricamento del modulo */
                /*------------------------*/
                $("#bfo_001").submit()    ;
            }
        });
    });          

    /*====================================*/
    /* Accettazione Barcode               */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('.acc_klb').change(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WB"                ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var ide_row = "#rw3_" +
                      pri_bfo             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var prt_bfo = $(ide_row).find('#prt').val();
        var prg_bfo = $(ide_row).find('#prg').val();
        var pri_bfo = $(ide_row).find('#pri').val();
        var klb_pro = $(ide_row).find('#klb').val();
        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm =
        {
            "rsp_des": rsp_des,
            "tip_ope": tip_ope,
            "prt_bfo": prt_bfo,
            "prg_bfo": prg_bfo,
            "pri_bfo": pri_bfo,
            "klb_pro": klb_pro
        }
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm)          ;
        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./elebfox0",
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* Eventuale messaggio    */
                /*------------------------*/
                $('#msg').html(data)      ;
                /*------------------------*/
                /* Caricamento del modulo */
                /*------------------------*/
                $("#bfo_001").submit()    ;
            }
        });
    });          





    /*====================================*/
    /* Bottone Foto                       */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('.img').click(function()
    {
        /*--------------------------------*/
        /* Identificatori riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_bfo             ;
        var prt_bfo = $(ide_row).find('#prt').val();
        var prg_bfo = $(ide_row).find('#prg').val();

        /*--------------------------------*/
        /* Richiamo modulo                */
        /*--------------------------------*/
        $.redirect("./elebfoi0.pl",
            {"prt_bfo":prt_bfo,
             "prg_bfo":prg_bfo},
             "POST",
             "_self")                     ;
    });

    /*====================================*/
    /* Bottone stampa                     */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $("#stp").click(function()
    {
        /*--------------------------------*/
        /* Protocollo                     */
        /*--------------------------------*/
        var prt_bfo = $('#prt').val()     ;
        /*--------------------------------*/
        /* Disabilitazione temporanea     */
        /*--------------------------------*/
//        $('#stp').prop("disabled",true)   ;
        /*--------------------------------*/
        /* Messaggio di stampa in corso   */
        /*--------------------------------*/
        var msg_stp = "<h1>Esecuzione " +
                      "stampa documento " +
                      "in corso ...</h1"  ;
        $('#msg').html(msg_stp)           ;
        /*--------------------------------*/
        /* Attesa di pochi secondi        */
        /*--------------------------------*/
        $('#stp').slideUp(1200).fadeIn(1200);
        /*--------------------------------*/
        /* Stampa                         */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo di stampa  */
            /*----------------------------*/
            type: "POST",
            url: "./elebfo1s",
//            contentType: "application/x-www-form-urlencoded",
            dataType: 'image/jpg',
            data: "prt_bfo=" +
                   prt_bfo,
            /*----------------------------*/
            /* Se andata a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* Esito                  */
                /*------------------------*/
                $('#msg').html(data)      ;
            }
        });
    });

/*----------------------------------------*/
/* Fine funzione livello '1'              */
/*========================================*/
}

/*========================================*/
/* Funzione di gestione livello '2'       */
/*                                        */
/* Input quantita' per ubicazione         */
/*----------------------------------------*/
function liv_002 ()
{
    /*====================================*/
    /* Focus su ubicazione e quantita'    */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#cod_ubi').focus()                 ;
    $('#cod_ubi').select()                ;

    var ubi = $('#ubi').val()             ;
    if (ubi != '')
    {
            $('.numbersOnly').focus()     ;
            $('.numbersOnly').select()    ;

    }

    /*====================================*/
    /* Bottone fine modifica              */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#but_fip').click(function()
    {
        /*--------------------------------*/
        /* A livello precedente           */
        /*--------------------------------*/
        $('#liv_ope').val('1')            ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#bfo_001").submit()            ;
    });

    /*====================================*/
    /* Bottone refresh                    */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#but_ref').click(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#bfo_001").submit()            ;
    });

    /*====================================*/
    /* Highlight quantita'                */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('.numbersOnly').click(function()
    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select()                  ;
    });

    /*====================================*/
    /* Rilevazione tasto funzione Tangram */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('.numbersOnly').keyup(function(e)
    {
        /*--------------------------------*/
        /* Rilevazione tasto              */
        /*--------------------------------*/
        var tgk = (e.keyCode ? 
	            e.keyCode : 
	            e.which)               ;
        /*--------------------------------*/
        /* Se 'invio' o 'return'          */
        /*--------------------------------*/
        if(tgk == 13)
          {$("#bfo_001").submit()}        ;
    });           

    /*====================================*/
    /* Modifica quantita'                 */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('.numbersOnly').change(function()
    {
        /*--------------------------------*/
        /* Codice ubicazione              */
        /*--------------------------------*/
        var cod_ubi = $(this).closest("tr").find('#ubi').val();
        /*--------------------------------*/
        /* Test preliminare se ubicazione */
        /* selezionata                    */
        /*--------------------------------*/
        if (cod_ubi =='')
        {
            /*----------------------------*/
            /* Messaggio                  */
            /*----------------------------*/
            alert ("Selezionare una ubicazione");
            /*----------------------------*/
            /* Normalizzazione quantita'  */
            /*----------------------------*/
            $('#qtv').val('')             ;
            $('#cod_ubi').focus()         ;
            $('#cod_ubi').select()        ;
            /*----------------------------*/
            /* Uscita                     */
            /*----------------------------*/
            return                        ;
        }
    
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WU"                ;
        /*--------------------------------*/
        /* Numero documento               */
        /*--------------------------------*/
        num_doc = $('#num_doc').val()     ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_bfo             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var rsp_doc = $('#rsp_doc').val();
        var prt_bfo = $(ide_row).find('#prt').val();
        var prg_bfo = $(ide_row).find('#prg').val();
        var cod_ubi = $(ide_row).find('#ubi').val();
        var qta_ord = $(ide_row).find('#qta').val();
        var qta_ubi = $(ide_row).find('#qtv').val();
        var num_pro = $(ide_row).find('#pro').val();
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
        if (qta_ubi == 0 && flg_spn == 'S')
           {alert("Quantita' a zero!");}  ;

        /*--------------------------------*/
        /* Messaggio di quantita' su-     */
        /* periore a quella in ordine     */
        /*--------------------------------*/
        var dif = qta_ord - qta_ubi       ;
        if (dif < 0)
        {
            /*----------------------------*/
            /* Normalizzazione            */
            /*----------------------------*/
            $(ide_row).find('#qtv').val(0);
            qta_ubi = 0                   ;
            /*----------------------------*/
            /* Messaggio                  */
            /*----------------------------*/
            alert("Quantita' non accettabile!");
            /*----------------------------*/
            /* Uscita                     */
            /*----------------------------*/
            return                        ;
        }

        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm =
        {
            "rsp_doc": rsp_doc,
            "tip_ope": tip_ope,
            "prt_bfo": prt_bfo,
            "prg_bfo": prg_bfo,
            "cod_ubi": cod_ubi,
            "num_pro": num_pro,
            "qta_ubi": qta_ubi
        }
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm)          ;

        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./elebfox0",
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* A livello precedente   */
                /*------------------------*/
 //               $('#liv_ope').val('2')    ;
                /*------------------------*/
                /* Ricarica modulo        */
                /*------------------------*/
                $("#bfo_001").submit()    ;
            }
        });
    });          

    /*====================================*/
    /* Accettazione quantita' verificata: */
    /* scorciatoia per copiare quantita'  */
    /* ordinata                           */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#rig td').click(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WU"                ;
        /*--------------------------------*/
        /* Numero documento               */
        /*--------------------------------*/
        num_doc = $('#num_doc').val()     ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_bfo             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var rsp_doc = $('#rsp_doc').val();
        var num_doc = $(ide_row).find('#prt').val();
        var prg_bfo = $(ide_row).find('#prg_bfo').val();
        var cod_ubi = $(ide_row).find('#ubi').val();
        var num_pro = $(ide_row).find('#pro').val();
        var num_cln = $(this).parent().children().index($(this));
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
        if (num_cln == 11)
        {
        
            /*----------------------------*/
            /* Test se ubicazione         */
            /*----------------------------*/
            if (cod_ubi =='')
            {
                alert ("Selezionare una ubicazione");
                $("#bfo_001").submit()    ;
            }
        
            /*----------------------------*/
            /* Copia della quantita'      */
            /*----------------------------*/
            var qta_sub = $(this).closest("tr").find('#qta').val();
            $(this).closest("tr").find('#qtv').val(qta_sub);

            /*----------------------------*/
            /* Oggetto con i Parametri    */
            /*----------------------------*/
            const prm =
            {
                "rsp_doc": rsp_doc,
                "tip_ope": tip_ope,
                "prt_bfo": prt_bfo,
                "prg_bfo": prg_bfo,
                "cod_ubi": cod_ubi,
                "num_pro": num_pro,
                "qta_ubi": qta_sub
            }
            /*----------------------------*/
            /* Assemblaggio parametri     */
            /*----------------------------*/
            const dat = $.param(prm)      ;

            /*----------------------------*/
            /* Aggiornamento Tangram      */
            /*----------------------------*/
            $.ajax({
                /*------------------------*/
                /* Richiamo modulo        */
                /*------------------------*/
                type: "POST",
                url: "./elebfox0",
                contentType: "application/x-www-form-urlencoded",
                data: dat,
                /*------------------------*/
                /* Se andato a buon fine  */
                /*------------------------*/
                success: function (data)
                {
                    /*------------------------*/
                    /* Ricarica modulo        */
                    /*------------------------*/
                    $("#bfo_001").submit()    ;
                }
            });
            
        }

    });

    /*====================================*/
    /* Controllo inserimento ubicazione   */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#cod_ubi').keydown(function(e)
    {
        /*--------------------------------*/
        /* Controllo se tasto enter       */
        /*--------------------------------*/
        if (e.keyCode === 13)
        {
            /*----------------------------*/
            /* Controllo se tasto enter   */
            /*----------------------------*/
            e.preventDefault()            ;
            /*----------------------------*/
            /* Valori da riga tabella     */
            /*----------------------------*/
            var cod_ubi = $('#cod_ubi').val();
            var num_pro = $('#num_pro').val();
            var prg_bfo = $('#prg_bfo').val();
            /*----------------------------*/
            /* Oggetto con i Parametri    */
            /*----------------------------*/
            const prm =
            {
                "cod_ubi": cod_ubi,
                "num_pro": num_pro
            }
            /*----------------------------*/
            /* Assemblaggio parametri     */
            /*----------------------------*/
            const dat = $.param(prm)      ;
            /*----------------------------*/
            /* Lettura                    */
            /*----------------------------*/
            $.ajax({ type: 'POST',
                      url: "./eleodsu0",
              contentType: "application/x-www-form-urlencoded",
                     data: dat,
                  success: function(data)
                {
                /*------------------------*/
                /* Valore in output       */
                /*------------------------*/
                let res = data.substring(0, 2);

                /*========================*/
                /* Se 'OK'                */
                /*------------------------*/
                if (res == 'OK')
                {
                    /*--------------------*/
                    /* Progressivo riga   */
                    /*--------------------*/
                    $('#prg_bfo').val(prg_bfo);
                    /*--------------------*/
                    /* Esecuzione modulo  */
                    /*--------------------*/
                    $("#bfo_001").submit();
                }
                /*========================*/
                /* Se non in giacenza     */
                /*------------------------*/
                else if (res == 'KU')
                {
                    /*--------------------*/
                    /* Messaggio          */
                    /*--------------------*/
                    alert('Prodotto non in ubicazione!');
                    /*--------------------*/
                    /* Normalizzazione    */
                    /* Ubicazione         */
                    /*--------------------*/
                    $('#cod_ubi').val('');
                    /*--------------------*/
                    /* Progressivo riga   */
                    /*--------------------*/
                    $('#prg_bfo').val(prg_bfo);
                    /*--------------------*/
                    /* Esecuzione modulo  */
                    /*--------------------*/
                    $("#bfo_001").submit();
                }
                /*========================*/
                /* Se inesistente         */
                /*------------------------*/
                 else
                {
                    /*--------------------*/
                    /* Messaggio          */
                    /*--------------------*/
                    alert('Ubicazione inesistente');
                    /*--------------------*/
                    /* Normalizzazione    */
                    /* Ubicazione         */
                    /*--------------------*/
                    $('#cod_ubi').val('') ;                
                }
            } 
            /*----------------------------*/
            /* Lettura                    */
            /*                            */
            /* Fine                       */
            /*----------------------------*/
            });
        /*--------------------------------*/
        /* Controllo se tasto enter       */
        /*                                */
        /* Fine                           */
        /*--------------------------------*/
        }
    /*------------------------------------*/
    /* Controllo inserimento ubicazione   */
    /*                                    */
    /* Livello '3'                        */
    /*                                    */
    /* Fine                               */
    /*====================================*/
    });

/*----------------------------------------*/
/* Fine funzione livello '2'              */
/*========================================*/
}

/*========================================*/
/* Funzione di richiesta conferma spunta  */
/*----------------------------------------*/
function cnf_dlg(msg)
{
    /*------------------------------------*/
    /* Messaggio per il box               */
    /*------------------------------------*/
    var dialog = $("<div>"+msg+"</div>")  ;
    /*------------------------------------*/
    /* Variabile per si/no                */
    /*------------------------------------*/
    var def = $.Deferred()                ;
    /*------------------------------------*/
    /* Creazione finestra di dialogo      */
    /*------------------------------------*/
    $(dialog).dialog(
    {
        /*--------------------------------*/
        /* Parametri box                  */
        /*--------------------------------*/
        resizable: false,
        autoOpen : true,
        title    : "Attenzione !",
        modal    : true,
        buttons:
        {
            'si': function()
            {
                def.resolve();
                $(dialog).dialog( "close" );
            },
            'NO': function()
            {
                def.reject();
                $(dialog).dialog( "close" );
            }
        },
        /*--------------------------------*/
        /* Bottone di default (NO)        */
        /*--------------------------------*/
        open: function()
        {
            $(this).siblings('.ui-dialog-buttonpane').find('button:eq(1)').focus(); 
        }
        
    });
    /*------------------------------------*/
    /* Variabile in uscita                */
    /*------------------------------------*/
    return def.promise()                  ;
/*----------------------------------------*/
/* Fine funzione                          */
/*========================================*/
}
