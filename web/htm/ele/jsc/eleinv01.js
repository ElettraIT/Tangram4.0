/*========================================*/
/* eleinv01 - Javascript - by WIP         */
/*                                        */
/* Elettra : Inventario per Ubicazione    */
/* -------------------------------------- */
/* Aggiornamento   : 04/01/2025           */
/*========================================*/

/*========================================*/
/* Variabili statiche                     */
/*----------------------------------------*/
mod_frm = "#inv_001"                      ;
url_nip = "../elebfoi0"                   ;
url_wrt = "../ele/eleinvw0"               ;
url_ubi = "../ele/eleinv11"               ;

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
    /* Test iniziale su indirizzo IP per  */
    /* individuare l'operatore            */
    /*                                    */
    /* Livello '0'                        */
    /*------------------------------------*/
    $.ajax({
        type: "POST",
        url : "../getip.pl",
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
    /* Focus su ubicazione                */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#cod_ubi').focus()                 ;
    $('#cod_ubi').select()                ;

    /*====================================*/
    /* Highlight ubicazione               */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#cod_ubi').click(function()
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
    /* Cambio ubicazione                  */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#cod_ubi').change(function()
    {
        /*--------------------------------*/
        /* Pulizia ubicazione             */
        /*--------------------------------*/
        var cod_ubi = $('#cod_ubi').val() ;
            cod_ubi = cod_ubi.trim()      ;
            cod_ubi = pul_str (cod_ubi)   ;
        $('#cod_ubi').val(cod_ubi)        ;
        var prm = 'cod_ubi=' + cod_ubi   ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });          

    /*====================================*/
    /* Bottone conferma                   */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#but_fip').click(function()
    {
        /*--------------------------------*/
        /* Test se presente ubicazione    */
        /*--------------------------------*/
//        var ubi = $("#cod_ubi").val()     ;
//        if (ubi != '')
//        {
            /*----------------------------*/
            /* A livello successivo       */
            /*----------------------------*/
            $('#liv_ope').val('2')        ;
            /*----------------------------*/
            /* Caricamento del modulo     */
            /*----------------------------*/
            $(mod_frm).submit()           ;
//        }
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
        /* Normalizzazione ubicazione     */
        /*--------------------------------*/
        $('#cod_ubi').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
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
    /* Definizione box di dialogo per     */
    /* conferma spunta totale             */
    /*------------------------------------*/
    $('#dialog-confirm').dialog({
      autoOpen: false,
      resizable: false,
      height: "auto",
      width: 400,
      modal: true,
      buttons: {
        "Vuoi spuntare tutti le righe rimanenti?": function() {
          $( this ).dialog( "close" );
        },
        Cancel: function() {
          $( this ).dialog( "close" );
        }
      },

        /*--------------------------------*/
        /* Default 'NO'                   */
        /* ___ NON FUNZIONA ___           */
        /*--------------------------------*/
      open: function() {
          $('#dialog-confirm.ui-dialog-buttonpane button:eq(1)').focus();
          }


    });    

    /*====================================*/
    /* Definizione box di dialogo -       */
    /* opzioni                            */
    /*                                    */
    /* ___ ATTUALMENTE INIBITO ___        */
    /*------------------------------------*/
    $('#dialog-confirm').dialog('option', 'buttons',
    {
        /*--------------------------------*/
        /* Scelta 'SI'                    */
        /*--------------------------------*/
        "si" : function()
        {
            /*----------------------------*/
            /* Responsabile               */
            /*----------------------------*/
            var rsp_doc = $('#rsp_doc').val();
            /*----------------------------*/
            /* Tipo operazione            */
            /*----------------------------*/
            var tip_ope = "WG"           ;
            /*----------------------------*/
            /* Numero distinta            */
            /*----------------------------*/
            var num_dst = $('#num_dst').val();
            /*----------------------------*/
            /* Parametri                  */
            /*----------------------------*/
            var prm_01 = "rsp_doc=" + rsp_doc ;
            var prm_02 = "tip_ope=" + tip_ope ;
            var prm_03 = "num_dst=" + num_dst ;
            /*----------------------------*/
            /* Assemblaggio parametri     */
            /*----------------------------*/
            var prm = prm_01 + "&" +
                      prm_02 + "&" +
                      prm_03              ;
            /*----------------------------*/
            /* Aggiornamento Tangram      */
            /*----------------------------*/
            $.ajax({
                /*------------------------*/
                /* Richiamo modulo        */
                /*------------------------*/
                type: "POST",
                url: "./eleodsw0",
                contentType: "application/x-www-form-urlencoded",
                data: prm,
                /*------------------------*/
                /* Se andato a buon fine  */
                /*------------------------*/
                success: function (data)
                {
                    /*--------------------*/
                    /* Messaggi           */
                    /*--------------------*/
                    $('#msg').html(data)  ;
                    /*--------------------*/
                    /* Ricarica modulo    */
                    /*--------------------*/
                    $(mod_frm).submit()   ;
                }
            });
        },
        /*--------------------------------*/
        /* Scelta 'NO'                    */
        /*--------------------------------*/
        "NO" : function()
        {
            /*----------------------------*/
            /* Box di dialogo: close      */
            /*----------------------------*/
            $(this).dialog("close")       ;
        }
    });

    /*====================================*/
    /* Recupero valori della variabile    */
    /* hidden in array                    */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    var ubi_vrt = $('#ubi_vrt').val()     ;
    var ubi_non = ubi_vrt.split(',').map(ubi => ubi.trim());

    /*====================================*/
    /* Colorazione di rosso ubicazione    */
    /* principale ed eventuale evidenza   */
    /* di codice e descrizione in presen- */
    /* za dei codici ubicazione 'B'       */
    /*                                    */
    /* (Giorgio 12/06/24)                 */
    /*                                    */
    /* Scansione di tutte le righe        */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#rig tr').each(function()
    {
        /*================================*/
        /* Riferimento alla riga          */
        /*--------------------------------*/
        var $row = $(this)                ;

        /*================================*/
        /* Estrazione literal ubicazione  */
        /*--------------------------------*/
        var lit_ubi = $row.find('#lit_ubi').text();
        /*================================*/
        /* Creazione array dal literal    */
        /*--------------------------------*/
        var ubi_arr = lit_ubi.split(',')  ;
        /*--------------------------------*/
        /* Controllo ubicazioni che       */
        /* iniziano con 'B'               */
        /*--------------------------------*/
        var ubi_bel = ubi_arr.some(function(e)
        {
            return e.trim().toLowerCase().startsWith('b');
        });
        /*--------------------------------*/
        /* Se trovato iniziale 'B'        */
        /*--------------------------------*/
        if (ubi_bel)
        {
            /*----------------------------*/
            /* Cambia il background delle */
            /* celle codice e descrizione */
            /*----------------------------*/
            $row.find('td').eq(1).css('background-color', 'lightblue');
            $row.find('td').eq(2).css('background-color', 'lightblue');
        }

        /*================================*/
        /* Estrazione ubicazione princi-  */
        /* pale                           */
        /*--------------------------------*/
        var pri_ubi = $row.find('#ubp').val();
        /*--------------------------------*/
        /* Pulizia literal ubicazione     */
        /*--------------------------------*/
        lit_ubi = lit_ubi.trim()          ;
        /*--------------------------------*/
        /* Pulizia literal ubicazione     */
        /*--------------------------------*/
        if (lit_ubi !== '')
        {
            /*----------------------------*/
            /* Individuazione eventuale   */
            /* ubicazione principale      */
            /*----------------------------*/
            if (lit_ubi.includes(pri_ubi)) 
            {
                /*------------------------*/
                /* Sostituzione ubicazione*/
                /* principale             */
                /*------------------------*/
                var inx_wrd = lit_ubi.replace(pri_ubi, `<span style="color: red;">${pri_ubi}</span>`);
                lit_ubi = inx_wrd         ;
                /*------------------------*/
                /* Scrittura nuova        */
                /* stringa con ubicazioni */
                /* in grassetto           */
                /*------------------------*/
                $row.find('#lit_ubi').html(lit_ubi);
                $row.find('#lit_ubi').css('font-weight', 'bold');
            }    
        } 
        else 
        {
            /*----------------------------*/
            /* Stringa vuota              */
            /*----------------------------*/
            $row.find('#lit_ubi').html('');
        }
        /*================================*/
        /* Ciclo per lettura righe ed     */
        /* eventuale colorazione grigia   */
        /*--------------------------------*/
        ubi_arr.forEach(function(e) {
            /*----------------------------*/
            /* Rimuovo caratteri laterali */
            /*----------------------------*/
            var ubi_trm = e.trim();
            /*----------------------------*/
            /* Se valore corrisponde a non*/
            /* modificabili, cambia colore*/
            /*----------------------------*/
            if (ubi_non.includes(ubi_trm.toUpperCase())) 
            {
                /*------------------------*/
                /* Sostituisco valore     */
                /* colorato in grigio     */
                /*------------------------*/
                var inx_wrd = lit_ubi.replace(ubi_trm, `<span style="color: grey;">${ubi_trm}</span>`);
                lit_ubi = inx_wrd         ;
                /*------------------------*/
                /* Scrittura nuova        */
                /* stringa con ubicazioni */
                /* in grassetto           */
                /*------------------------*/
                $row.find('#lit_ubi').html(lit_ubi);
                $row.find('#lit_ubi').css('font-weight', 'bold');
            }
        });

    /*------------------------------------*/
    /* Colorazione di rosso ubicazione    */
    /*                                    */
    /* Livello '1'                        */
    /*                                    */
    /* Fine                               */
    /*====================================*/
    });

    /*====================================*/
    /* Focus su prodotto                  */
    /*------------------------------------*/
    $('#alf_pro').focus()                 ;
    $('#alf_pro').select()                ;

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
    /* Cambio Prodotto da verificare      */
    /*------------------------------------*/

    /*====================================*/
    /* Controllo click su elementi riga   */
    /*------------------------------------*/
    $('#rig td').click(function()
    {
        /*--------------------------------*/
        /* Indicatori colonne             */
        /*--------------------------------*/
        var cln_pro = 12                  ;
        var cln_qta = cln_pro + 3         ;
        var cln_qrl = cln_pro + 6         ;
        var cln_ubi = cln_pro + 7         ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_inv = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_inv             ;
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
        /* Se colonna codice prodotto     */
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
        /* Se colonna della quantita'     */
        /* rilevata e non ci sia gia' la  */
        /* spunta                         */
        /*--------------------------------*/
        if (num_cln == cln_qta)
        {
            /*----------------------------*/
            /* Copia della quantita'      */
            /*----------------------------*/
            if (flg_spn == 'N')
            {
                var qta = $(this).closest("tr").find('#res').val();
                $(this).closest("tr").find('#qtv').val(qta);
            }
        }

        /*================================*/
        /* Se colonna della quantita'     */
        /* rilevata in altre ubicazioni   */
        /*--------------------------------*/
         else if (num_cln == cln_qrl)
        {
            /*----------------------------*/
            /* Tipo operazione            */
            /*----------------------------*/
            var tip_ope = "RL"            ;
            /*----------------------------*/
            /* Codice prodotto            */
            /*----------------------------*/
            var alf_pro = $(ide_row).find('#alf').val();
            /*----------------------------*/
            /* Oggetto con i Parametri    */
            /*----------------------------*/
            const prm =
            {
                "tip_ope": tip_ope,
                "alf_pro": alf_pro
            }
            /*----------------------------*/
            /* Assemblaggio parametri     */
            /*----------------------------*/
            const dat = $.param(prm)      ;
            /*----------------------------*/
            /* Titolo box                 */
            /*----------------------------*/
            var tit = "QUANTITA' RILEVATA - Prodotto : " + alf_pro;
            /*----------------------------*/
            /* Riempimento box            */
            /*----------------------------*/
            $.ajax({
                type: "POST",
                url: url_ubi,
                contentType: "application/x-www-form-urlencoded",
                data: dat,
                success: function (data)
                {
                    $("#dialog").html(data);
                    $("#dialog").dialog({
                        title: tit,
                        open: function() {
                            // Aggiungi classe per titolo blu solo per questa situazione specifica
                            $(this).parent().find(".ui-dialog-titlebar")
                                .addClass("ui-dialog-titlebar-blu");
                        }
                    }).dialog('open');
                }
            });
        }
        
        /*================================*/
        /* Se colonna dell'ubicazione     */
        /*--------------------------------*/
         else if (num_cln == cln_ubi)
        {
            /*----------------------------*/
            /* Tipo operazione            */
            /*----------------------------*/
            var tip_ope = "PR"            ;
            /*----------------------------*/
            /* Codice prodotto            */
            /*----------------------------*/
            var alf_pro = $(ide_row).find('#alf').val();
            /*----------------------------*/
            /* Oggetto con i Parametri    */
            /*----------------------------*/
            const prm =
            {
                "tip_ope": tip_ope,
                "alf_pro": alf_pro
            }
            /*----------------------------*/
            /* Assemblaggio parametri     */
            /*----------------------------*/
            const dat = $.param(prm)      ;
            /*----------------------------*/
            /* Titolo box                 */
            /*----------------------------*/
            var tit = "QUANTITA' PRESUNTA - Prodotto : " + alf_pro;
            /*----------------------------*/
            /* Riempimento box            */
            /*----------------------------*/
            $.ajax({
                type: "POST",
                url: url_ubi,
                contentType: "application/x-www-form-urlencoded",
                data: dat,
                success: function (data)
                {
                      $("#dialog").html(data);
                      $("#dialog").dialog({
                        title: tit,
                        open: function() {
                            // Rimuove classe per titolo blu 
                            $(this).parent().find(".ui-dialog-titlebar")
                                .removeClass("ui-dialog-titlebar-blu");
                        }
                    }).dialog('open');
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
    /* Highlight prodotto                 */
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
    /* Modifica box di spunta o quantita''*/
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
        var cod_ubi = $('#cod_ubi').val();
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

        /*================================*/
        /* Parametri                      */
        /*--------------------------------*/
        const par = []                    ;
        const val = []                    ;
        /*--------------------------------*/
        par[0] = "rsp_doc"                ;
        val[0] = rsp_doc                  ;
        /*--------------------------------*/
        par[1] = "tip_ope"                ;
        val[1] = tip_ope                  ;
        /*--------------------------------*/
        par[2] = "cod_ubi"                ;
        val[2] = cod_ubi                  ;
        /*--------------------------------*/
        par[3] = "alf_pro"                ;
        val[3] = alf_pro                  ;
        /*--------------------------------*/
        par[4] = "num_pro"                ;
        val[4] = num_pro                  ;
        /*--------------------------------*/
        par[5] = "qta_prs"                ;
        val[5] = qta_prs                  ;
        /*--------------------------------*/
        par[6] = "qta_rlv"                ;
        val[6] = qta_rlv                  ;
        /*--------------------------------*/
        par[7] = "flg_spn"                ;
        val[7] = flg_spn                  ;
        /*--------------------------------*/
        /* Assemblaggio                   */
        /*--------------------------------*/
        prm = ass_prm(par, val)           ;
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
            data: prm,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                $('#msg').html(data)      ;
                /*------------------------*/
                /* Focus su prodotto      */
                /*------------------------*/
                $('#alf_pro').focus()     ;
                /*------------------------*/
                /* Caricamento del modulo */
                /*------------------------*/
                $(mod_frm).submit()       ;
            }
        });
    });          

    /*====================================*/
    /* Bottone fine spunta ubicazione     */
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
    /*------------------------------------*/
    $('#but_ref').click(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });
    
    /*====================================*/
    /* Bottone completamento spunte       */
    /*------------------------------------*/
    $('#but_gen').click(function()
    {
        /*--------------------------------*/
        /* ATTUALMENTE INIBITO            */
        /*--------------------------------*/
        alert("Funzione non disponibile");
        return;

        /*--------------------------------*/
        /* Calcolo righe da spuntare      */
        /*--------------------------------*/
        var rig = 0                       ;
        var spt = 0                       ;
        var nsp = 0                       ;
        $("#rig tr").each(function()
        {
            var unc = $(this).find('input[type="checkbox"]:not(:checked)');
            if (unc.length)
            {nsp++} else {spt++}          ;
            rig++                         ;
        });                  
        var dsp = rig - spt               ;
        /*--------------------------------*/
        /* Messaggio conferma operazione  */
        /*--------------------------------*/
        var res_msg = ''                  ;
        
        if (dsp == 1)
        {
            res_msg = '<h1>Vuoi veramente spuntare la riga rimanente?</h1>';
        }
         else
        {
            res_msg = '<h1>Vuoi veramente spuntare le ' + dsp + ' righe rimanenti?</h1>';
        }
        /*--------------------------------*/
        /* Box di conferma operazione     */
        /*--------------------------------*/
        $('#dialog-confirm').html(res_msg);
        $('#dialog-confirm').dialog("option","title","ATTENZIONE !").dialog('open');
    });

    /*====================================*/
    /* Bottone Non conformita'            */
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
        var cod_ubi = $('#cod_ubi').val();
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
        
        /*================================*/
        /* Parametri                      */
        /*--------------------------------*/
        const par = []                    ;
        const val = []                    ;
        /*--------------------------------*/
        par[0] = "rsp_doc"                ;
        val[0] = rsp_doc                  ;
        /*--------------------------------*/
        par[1] = "tip_ope"                ;
        val[1] = tip_ope                  ;
        /*--------------------------------*/
        par[2] = "cod_ubi"                ;
        val[2] = cod_ubi                  ;
        /*--------------------------------*/
        par[3] = "alf_pro"                ;
        val[3] = alf_pro                  ;
        /*--------------------------------*/
        par[4] = "num_pro"                ;
        val[4] = num_pro                  ;
        /*--------------------------------*/
        par[5] = "qta_prs"                ;
        val[5] = qta_prs                  ;
        /*--------------------------------*/
        par[6] = "qta_rlv"                ;
        val[6] = qta_rlv                  ;
        /*--------------------------------*/
        par[7] = "flg_spn"                ;
        val[7] = flg_spn                  ;
        /*--------------------------------*/
        par[8] = "ann_ncf"                ;
        val[8] = ann_ncf                  ;
        /*--------------------------------*/
        /* Assemblaggio                   */
        /*--------------------------------*/
        prm = ass_prm(par, val)           ;
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
                $(mod_frm).submit()       ;
            }
        });
    });          

    /*====================================*/
    /* Forzatura caratteri ASCII per il   */
    /* campo testo della Non conformita'  */
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
/* Funzione per assemblaggio parametri    */
/*----------------------------------------*/
function ass_prm(nom, val)
{
    /*------------------------------------*/
    /* Normalizzazioni                    */
    /*------------------------------------*/
    let inx = 0                           ;
    let par = ''                          ;
    /*------------------------------------*/
    /* Ciclo                              */
    /*------------------------------------*/
    while (inx < nom.length)
    {
        /*--------------------------------*/
        /* Assemblaggio                   */
        /*--------------------------------*/
        par = par 
            + nom[inx]
            + "=" 
            + val[inx]
            + "&"                         ;
        /*--------------------------------*/
        /* Incremento indice              */
        /*--------------------------------*/
        inx++;
    }        
    /*------------------------------------*/
    /* Valore in uscita                   */
    /*------------------------------------*/
    return par                            ;
/*----------------------------------------*/
/* Funzione richiesta inserimento - fine  */
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
              url: url_nip,
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
/* Funzione di pulizia stringa            */
/*                                        */
/* N.B.: usata per togliere caratteri non */
/*       alfabetici o numerici            */
/*----------------------------------------*/
function pul_str(inp_str)
{
    return inp_str.replace(/[^a-zA-Z0-9]/g, '');
};    

