/*========================================*/
/* eleinv02 - Javascript - by WIP         */
/*                                        */
/* Elettra : Inventario per Classe        */
/* -------------------------------------- */
/* Aggiornamento   : 04/01/2025           */
/*========================================*/

/*========================================*/
/* Variabili statiche                     */
/*----------------------------------------*/
mod_frm = "#inv_002"                      ;
url_rsp = "../getip.pl"                   ;
url_wrt = "../ele/eleinvw0"               ;
url_ubi = "../ele/eleinv11"               ;
url_fnd = "../ele/eleproA1.pl"            ;
url_pro = "../ele/eleinv03"               ;

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
    /* Definizione box di dialogo per     */
    /* espansione elenco 'check'          */
    /*------------------------------------*/
    $("#dialog").dialog({
        autoOpen: false,
        modal: true,
        title: 'Scheda prodotto',
        width: 950,
        height: 300,
    });

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
/* Funzione di gestione livello '0'       */
/*                                        */
/* Input Operatore                        */
/*----------------------------------------*/
function liv_000 ()
{
    /*====================================*/
    /* Test iniziale su indirizzo IP per  */
    /* individuare l'operatore            */
    /*                                    */
    /* Livello '0'                        */
    /*------------------------------------*/
    $.ajax({
        type: "POST",
        url : url_rsp,
        contentType: "application/x-www-form-urlencoded",
        /*--------------------------------*/
        /* Se indirizzo determinato       */
        /*--------------------------------*/
        success: function (data)
        {
            /*----------------------------*/
            /* Determinazione operatore   */
            /*----------------------------*/
            var ipx_val = data.trim()     ;
            let_ipx(ipx_val)              ;
        }
    });

    /*====================================*/
    /* Cella di accettazione in rosso     */
    /*------------------------------------*/
    cel_css("cel_ute", "R")               ;

    /*====================================*/
    /* Cambio Operatore                   */
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
        $(mod_frm).submit()               ;
    });          

/*----------------------------------------*/
/* Fine funzione livello '0'              */
/*========================================*/
}

/*========================================*/
/* Funzione di gestione livello '1'       */
/*                                        */
/* Input prodotti                         */
/*----------------------------------------*/
function liv_001 ()
{

    /*====================================*/
    /* Focus su classe                    */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#cod_cla').focus()                 ;
    $('#cod_cla').select()                ;

    /*====================================*/
    /* Highlight classe                   */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#cod_cla').click(function()
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
    /* Cambio classe                      */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#cod_cla').change(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });          

    /*====================================*/
    /* Cambio gruppo                      */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#cod_gru').change(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });          

    /*====================================*/
    /* Cambio sottogruppo                 */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#cod_sgr').change(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });          

    /*====================================*/
    /* Bottone conferma protocolli        */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#but_fip').click(function()
    {
        /*--------------------------------*/
        /* Test se presente ubicazione    */
        /*--------------------------------*/
        var ubi = $("#cod_ubi").val()     ;
        if (ubi != '')
        {
            /*----------------------------*/
            /* A livello successivo       */
            /*----------------------------*/
            $('#liv_ope').val('2')        ;
            /*----------------------------*/
            /* Caricamento del modulo     */
            /*----------------------------*/
            $(mod_frm).submit()           ;
        }
    });

    /*====================================*/
    /* Bottone annullamento               */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#but_fsu').click(function()
    {
        /*--------------------------------*/
        /* A livello precedente           */
        /*--------------------------------*/
        $('#liv_ope').val('1')            ;
        /*--------------------------------*/
        /* Normalizzazione classe         */
        /*--------------------------------*/
        $('#cod_cla').val('')             ;
        /*--------------------------------*/
        /* Normalizzazione gruppo         */
        /*--------------------------------*/
        $('#cod_gru').val('')             ;
        /*--------------------------------*/
        /* Normalizzazione sottogruppo    */
        /*--------------------------------*/
        $('#cod_sgr').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });

    /*====================================*/
    /* Bottone rientro menu               */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#but_mnu').click(function()
    {
        /*--------------------------------*/
        /* A livello precedente           */
        /*--------------------------------*/
        $(location).prop('href', '../../ele_inv.html')
    });

/*----------------------------------------*/
/* Fine funzione livello '1'              */
/*========================================*/
}

/*========================================*/
/* Funzione di gestione livello '2'       */
/*                                        */
/* Input Prodotti                         */
/*----------------------------------------*/
function liv_002 ()
{



    /*====================================*/
    /* INIBIZIONE TOTALE PROVVISORIA      */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $(":input[type=text]").prop("disabled", true);
    $(":input[type=number]").prop("disabled", true);
    $(":input[type=checkbox]").prop("disabled", true);





    /*====================================*/
    /* Tooltip per Classificazione        */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
   $('#div_cla').tooltip()               ;

    /*====================================*/
    /* Focus su prodotto                  */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#alf_pro').focus()                 ;
    $('#alf_pro').select()                ;

    /*====================================*/
    /* Cambio prodotto                    */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#alf_pro').change(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });          

    /*====================================*/
    /* Bottone reset                      */
    /*                                    */
    /* Livello '2'                        */
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
    /* Bottone find                       */
    /*                                    */
    /* Livello '2'                        */
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
        /* Focus su prodotto              */
        /*--------------------------------*/
        $("#alf_pro").focus()             ;
        
        /*--------------------------------*/
        /* Valore classe                  */
        /*--------------------------------*/
        var cla = $('#cod_cla').val()     ;
        /*--------------------------------*/
        /* Valore gruppo                  */
        /*--------------------------------*/
        var gru = $('#cod_gru').val()     ;
        /*--------------------------------*/
        /* Valore sottogruppo             */
        /*--------------------------------*/
        var sgr = $('#cod_sgr').val()     ;
        
        /*--------------------------------*/
        /* Attivazione Autocomplete       */
        /*--------------------------------*/
        $("#alf_pro").autocomplete(
        {
            /*----------------------------*/
            /* Esecuzione Autocomplete    */
            /*----------------------------*/
            source     : url_fnd
                       + "?cod_cla="
                       + cla
                       + "&cod_gru="
                       + gru
                       + "&cod_sgr="
                       + sgr,
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
                $(mod_frm).submit()       ;
            },
        });
    });

    /*====================================*/
    /* Controllo click su elementi riga   */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#rig td').click(function()
    {
        /*--------------------------------*/
        /* Indicatori colonne             */
        /*--------------------------------*/
        var cln_pro = 9                   ;
        var cln_des = cln_pro + 1         ;
        var cln_qta = cln_pro + 3         ;
        var cln_ubi = cln_pro + 6         ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_inv = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_inv             ;
                      
        /*--------------------------------*/
        /* Pulizia highlight campo quan-  */
        /* tita' di cuascuna riga         */
        /*--------------------------------*/
        $("#rig tr").each(function()
        {
            $(this).closest("tr").css("background-color","#FFF");
        });                  
        /*--------------------------------*/
        /* Highlight campo quantita'      */
        /*--------------------------------*/
        $(this).closest("tr").css("background-color","#CCC");
                      
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
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

        /*================================*/
        /* Test che sia la colonna del    */
        /* codice prodotto                */
        /*--------------------------------*/
        if (num_cln == cln_pro)
        {
            /*----------------------------*/
            /* Copia del codice           */
            /*----------------------------*/
            var codice = $(this).closest("tr").find('#alf').val();
            $('#alf_pro').val(codice)     ;
            /*----------------------------*/
            /* Caricamento del modulo     */
            /*----------------------------*/
            $(mod_frm).submit()           ;
        }

        /*================================*/
        /* Test che sia la colonna della  */
        /* descrizione                    */
        /*--------------------------------*/
        if (num_cln == cln_des)
        {
            /*----------------------------*/
            /* Codice prodotto            */
            /*----------------------------*/
            var rsp_doc = $('#rsp_doc').val();
            var alf_pro = $(this).closest("tr").find('#alf').val();
            /*----------------------------*/
            /* Parametri da passare       */
            /*----------------------------*/
            var par_frm = {
                "rsp_doc": rsp_doc,
                "alf_pro": alf_pro
            };
            /*----------------------------*/
            /* Apri dialog con iframe     */
            /*----------------------------*/
            dlg_ifr(1100, 750, alf_pro)   ;
            /*----------------------------*/
            /* Invia form dinamico        */
            /*----------------------------*/
            inv_frm(url_pro, par_frm, "ifr_pro");
        }

        /*================================*/
        /* Test che sia la colonna della  */
        /* quantita' ordinata e non ci    */
        /* sia gia' la spunta             */
        /*--------------------------------*/
        if (num_cln == cln_qta)
        {
            /*----------------------------*/
            /* Test se riga spuntata      */
            /*----------------------------*/
            if (flg_spn == 'N')
            {
                /*------------------------*/
                /* Copia della quantita'  */
                /*------------------------*/
                var qta = $(this).closest("tr").find('#qta').val();
                $(this).closest("tr").find('#qtv').val(qta);
            }
        }
        
        /*================================*/
        /* Test che sia la colonna della  */
        /* ubicazione                     */
        /*--------------------------------*/
         else if (num_cln == cln_ubi)
        {
            /*----------------------------*/
            /* Codice prodotto            */
            /*----------------------------*/
            var cod = $(ide_row).find('#alf').val();
            /*----------------------------*/
            /* Riempimento box            */
            /*----------------------------*/
            $.ajax({
                type: "POST",
                url : url_ubi,
                contentType: "application/x-www-form-urlencoded",
                data: "alf_pro=" + cod,
                success: function (data)
                {
                      $("#dialog").html(data);
                      $("#dialog").dialog("option","title","Prodotto : " + cod).dialog('open');
                }
            });
        }

    /*------------------------------------*/
    /* Controllo click su elementi riga   */
    /*                                    */
    /* Fine                               */
    /*====================================*/
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
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        cel_css("cel_num", "+")           ;
    });

    /*====================================*/
    /* Highlight prodotto                 */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#cel_pro').click(function()
    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select()                  ;
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        cel_css("cel_pro", "+")           ;
    });

    /*====================================*/
    /* Modifica box di spunta o quantita''*/
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('input[type="checkbox"],.numbersOnly').change(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WI"                ;
        /*--------------------------------*/
        /* Identificatori riga tabella    */
        /*--------------------------------*/
        var pri_inv = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_inv             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var rsp_doc = $('#rsp_doc').val();
        var cod_ubi = $(ide_row).find('#ubi').val();
        var alf_pro = $(ide_row).find('#alf').val();
        var num_pro = $(ide_row).find('#pro').val();
        var qta_prs = $(ide_row).find('#qta').val();
        var qta_rlv = $(ide_row).find('#qtv').val();
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
        if (qta_rlv == 0 && flg_spn == 'S')
           {alert("Quantita' a zero!");}  ;

        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm =
        {
            "rsp_doc": rsp_doc,
            "tip_ope": tip_ope,
            "cod_ubi": cod_ubi,
            "alf_pro": alf_pro,
            "num_pro": num_pro,
            "qta_prs": qta_prs,
            "qta_rlv": qta_rlv,
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
            url: url_wrt,
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
//                $('#msg').html(data)      ;
                /*------------------------*/
                /* Focus su prodotto      */
                /*------------------------*/
                $('#alf_pro').focus()     ;
                /*------------------------*/
                /* Ricarica modulo        */
                /*------------------------*/
//                $(mod_frm).submit()    ;
            }
        });
    });          

    /*====================================*/
    /* Bottone fine spunta ubicazione     */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#but_fsu').click(function()
    {
        /*--------------------------------*/
        /* A livello precedente           */
        /*--------------------------------*/
        $('#liv_ope').val('1')            ;
        /*--------------------------------*/
        /* Normalizzazione ubicazione     */
        /*--------------------------------*/
        $('#cod_ubi').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
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
        $(mod_frm).submit()               ;
    });
    
    /*====================================*/
    /* Bottone Non conformita'            */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('.ncf').click(function()
    {
        /*--------------------------------*/
        /* Identificatori riga tabella    */
        /*--------------------------------*/
        var pri_inv = $(this).closest("tr").find('#pri').val();
        var ide_rw2 = "#rw2_" +
                      pri_inv             ;
        /*--------------------------------*/
        /* Apertura / chiusura riga       */
        /*--------------------------------*/
        $(ide_rw2).toggle()               ;
        /*--------------------------------*/
        /* Focus                          */
        /*--------------------------------*/
        $(this).closest("tr").find('#nct').focus();
    });

    /*====================================*/
    /* Accettazione testo Non conformita' */
    /*                                    */
    /* Livello '2'                        */
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
        var pri_inv = $(this).closest("tr").find('#pri').val();
        var ide_row = "#rw2_" +
                      pri_inv             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var rsp_doc = $('#rsp_doc').val();
        var cod_ubi = $(ide_row).find('#ubi').val();
        var alf_pro = $(ide_row).find('#alf').val();
        var num_pro = $(ide_row).find('#pro').val();
        var qta_prs = $(ide_row).find('#qta').val();
        var qta_rlv = $(ide_row).find('#qtv').val();
        var ann_ncf = $(ide_row).find('#nct').val();
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
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm =
        {
            "rsp_doc": rsp_doc,
            "tip_ope": tip_ope,
            "cod_ubi": cod_ubi,
            "alf_pro": alf_pro,
            "num_pro": num_pro,
            "qta_prs": qta_prs,
            "qta_rlv": qta_rlv,
            "flg_spn": flg_spn,
            "ann_ncf": ann_ncf
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
            url: url_wrt,
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
                $(mod_frm).submit()       ;
            }
        });
    });          

    /*====================================*/
    /* Forzatura caratteri ASCII per il   */
    /* campo testo della Non conformita'  */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $("#nct").on("keypress",function(e)
    {
        if((e.which >= 48 && e.which <= 90) ||
           (e.which >= 97 && e.which <= 122) ||
            e.which == 45 || e.which == 32 || e.which == 39 || 
           (e.which >= 48 && e.which <= 57 ))
        {
            return true;
        }
         else
        {
            return false;
        }
    });

/*----------------------------------------*/
/* Fine funzione livello '2'              */
/*========================================*/
}

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
/* Lettura nodo IP per determinare il     */
/* codice operatore                       */
/*----------------------------------------*/
function let_ipx(val_ipx)
{
    /*------------------------------------*/
    /* Normalizzazione nodo IP            */
    /*------------------------------------*/
    var ipx_arr = val_ipx.split(".")      ;
    var ipw = ipx_arr[3]                  ;
    var ipw = pad(ipw, 3)                 ;
    /*------------------------------------*/
    /* Oggetto con i Parametri            */
    /*------------------------------------*/
    const prm =
    {
        "val_ipx": ipw
    }
    /*------------------------------------*/
    /* Assemblaggio parametri             */
    /*------------------------------------*/
    const dat = $.param(prm)              ;
    /*------------------------------------*/
    /* Lettura                            */
    /*------------------------------------*/
    $.ajax({ type: 'POST',
              url: "../elebfoi0",
      contentType: "application/x-www-form-urlencoded",
             data: dat,
          success: function(data)
          {
              /*--------------------------*/
              /* Esito lettura            */
              /*--------------------------*/
              let res = data.substring(0, 3);
                  res = res.trim()        ;
                  res = parseInt(res)     ;
                  $('#rsp_doc').val(res)  ;

              /*--------------------------*/
              /* Test su esito lettura    */
              /*--------------------------*/
              if (res == 0 || !res)
              {
                  /*----------------------*/
                  /* Se non trovato: nor- */
                  /* malizzazione codice  */
                  /* operatore            */
                  /*----------------------*/
                  $('#rsp_doc').val(0)    ;
                  /*----------------------*/
                  /* Uscita               */
                  /*----------------------*/
                    return                ;
              }
               else
              {
                  /*----------------------*/
                  /* Operatore determina- */
                  /* to                   */
                  /*----------------------*/
                  $('#rsp_doc').val(res)  ;
                  /*----------------------*/
                  /* A livello successivo */
                  /*----------------------*/
                  $('#liv_ope').val('1')  ;
                  $(mod_frm).submit()     ;
              }
          }
    });
/*----------------------------------------*/
/* Lettura nodo IP - fine                 */
/*========================================*/
}

/*========================================*/
/* Formattazione numeri con zeri in testa */
/*----------------------------------------*/
function pad (str, max)
{
    str = str.toString()                  ;
    return str.length < max ? pad("0" + str, max) : str;
}

/*========================================*/
/* Funzione per creare e gestire un       */
/* Dialog con iframe                      */
/*----------------------------------------*/
function dlg_ifr( wdt_dlg, hgt_dlg, pro_dlg)
{
    /*------------------------------------*/
    /* Creazione del contenitore del      */
    /* dialog con iframe                  */
    /*------------------------------------*/
    var dlg_prm = '<iframe '
                + 'id="ifr_pro" '
                + 'name="ifr_pro" '
                + 'style="border:none;" '
                + 'width="100%" '
                + 'height="100%">'
                + '</iframe>'             ;
    var dlg_div = $('<div></div>').append(dlg_prm);

    /*------------------------------------*/
    /* Impostazione del dialog con iframe */
    /*------------------------------------*/
    dlg_div.dialog({
        title    : "Dettagli Prodotto " + pro_dlg,
        autoOpen : true,
        width    : wdt_dlg || 800,  
        height   : hgt_dlg || 600, 
        modal    : true,
        resizable: false,
        close    : function()
        {
            /*----------------------------*/
            /* Distruzione del dialog al  */
            /* chiusura                   */
            /*----------------------------*/
            $(this).dialog('destroy').remove();
            /*----------------------------*/
            /* Ricarica del form          */
            /* chiusura                   */
            /*----------------------------*/
            $(mod_frm).submit()           ;
        }
    });
}

/*========================================*/
/* Funzione per creare e inviare un form  */
/* dinamico in POST                       */
/*----------------------------------------*/
function inv_frm (url, prm, frm)
{
    /*------------------------------------*/
    /* Creazione del form dinamico        */
    /*------------------------------------*/
    var form = $('<form>', {
        "method": "POST",
        "action": url,
        "target": frm
    });

    /*------------------------------------*/
    /* Aggiunta dei parametri al form     */
    /*------------------------------------*/
    $.each(prm, function(name, value) {
        form.append($('<input>', {
            "type" : "hidden",
            "name" : name,
            "value": value
        }));
    });

    /*------------------------------------*/
    /* Aggiunta del form al body e invio  */
    /*------------------------------------*/
    $('body').append(form)                ;
    form.submit()                         ;
    /*------------------------------------*/
    /* Rimozione del form dopo l'invio    */
    /*------------------------------------*/
    form.remove()                         ;
}

