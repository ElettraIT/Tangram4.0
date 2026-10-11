/*========================================*/
/* eleinv00 - Javascript - by WIP         */
/*                                        */
/* Elettra : Controllo inventario         */
/* -------------------------------------- */
/* Aggiornamento   : 04/12/2025           */
/*========================================*/
/*========================================*/
/* Variabili statiche                     */
/*----------------------------------------*/
mod_frm = "#inv_000"                      ;
url_wrt = "../ele/eleinvw0"               ;
url_ubi = "../ele/eleinv11"               ;
url_fnd = "../ele/eleproA0.pl"            ;
url_grf = "../ele/eleinvh1"               ;
url_rsp = "../ele/eleinvr1"               ;

/*========================================*/
/* Controllo dei dati                     */
/*----------------------------------------*/
$(document).ready(function()
{
    /*====================================*/
    /* Definizione per Radio buttons      */
    /*------------------------------------*/
    $("input[type=radio]").checkboxradio();

    /*====================================*/
    /* Variabili globali                  */
    /*------------------------------------*/
    /* Livello operativo                  */
    /*                                    */
    /* - '0' : Login utente e password    */
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
/* Login                                  */
/*----------------------------------------*/
function liv_000 ()
{

    /*====================================*/
    /* Cella di accettazione in rosso     */
    /*------------------------------------*/
    cel_css("cel_ute", "R")               ;

    /*====================================*/
    /* Lettura cookie utente e password   */
    /*------------------------------------*/
    var cki_usr = Cookies.get('cod_ute')  ;
    var cki_pwd = Cookies.get('pwd_ute')  ;

    /*====================================*/
    /* Highlight utente                   */
    /*                                    */
    /* Livello '0'                        */
    /*------------------------------------*/
    $('#cod_ute').click(function()
    {
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        cel_css("cel_ute", "R")           ;
        cel_css("cel_pwd", "-")           ;
    });

    /*====================================*/
    /* Highlight password                 */
    /*                                    */
    /* Livello '0'                        */
    /*------------------------------------*/
    $('#pwd_ute').click(function()
    {
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        cel_css("cel_ute", "-")           ;
        cel_css("cel_pwd", "R")           ;
    });

    /*====================================*/
    /* Controllo se i cookie esistono     */
    /*------------------------------------*/
    if (cki_usr && cki_pwd) 
    {
        $('#cod_ute').val(cki_usr)         ;
        $('#pwd_ute').val(cki_pwd)         ;
        $(mod_frm).submit()                ;
    }
    else
    {
        /*================================*/
        /* Focus su utente o password     */
        /*--------------------------------*/
        if ($('#cod_ute').val() !== '') 
        {
            $('#pwd_ute').prop("enabled",true);
            $('#pwd_ute').focus()         ;
            cel_css("cel_ute", "-")       ;
            cel_css("cel_pwd", "R")       ;
        }
        else
        {
            $('#cod_ute').focus()         ;
            $('#pwd_ute').prop("disabled",true);
        }
        /*================================*/
        /* Cambio Utente o Password       */
        /*--------------------------------*/
        $('#cod_ute, #pwd_ute').change(function() 
        {
            /*----------------------------*/
            /* Caricamento del modulo     */
            /*----------------------------*/
            $(mod_frm).submit()           ;
        });

    }

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
    /* Login passato, salvo i cookies     */
    /*------------------------------------*/
    var cki_usr = $('#cod_ute').val()     ;
    var cki_pwd = $('#pwd_ute').val()     ;
    /*------------------------------------*/
    /* Scrittura dei cookie               */
    /*------------------------------------*/
    Cookies.set('cod_ute', cki_usr, { expires: 1, path: '/' }) ;
    Cookies.set('pwd_ute', cki_pwd, { expires: 1, path: '/' }) ;

    /*====================================*/
    /* Definizione box di dialogo per     */
    /* messaggi di errore                 */
    /*------------------------------------*/
    $("#msg_err").dialog({
        autoOpen: false,
        modal   : true,
        title   : 'ATTENZIONE !',
        width   : 600,
        height  : "auto",
    });

    /*====================================*/
    /* Focus in base al tipo ordinamento  */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    var tip_ord = $("input[name='tip_ord']:checked").val();
    /*------------------------------------*/
    /* Deviazione in funzione del tipo di */
    /* selezione                          */
    /*------------------------------------*/
    switch(tip_ord)
    {
        /*================================*/
        /* Nessuna selezione              */
        /*--------------------------------*/
        case '':
            /*----------------------------*/
            /* Oltre                      */
            /*----------------------------*/
            break                         ;
        /*================================*/
        /* Prodotto                       */
        /*--------------------------------*/
        case 'P':
            /*----------------------------*/
            /* Focus su prodotto          */
            /*----------------------------*/
            $("#alf_pro").focus()         ;
            /*----------------------------*/
            /* Oltre                      */
            /*----------------------------*/
            break                         ;
        /*================================*/
        /* Ubicazione                     */
        /*--------------------------------*/
        case 'U':
            /*----------------------------*/
            /* Focus su ubicazione        */
            /*----------------------------*/
            $("#cod_ubi").focus()         ;
            /*----------------------------*/
            /* Oltre                      */
            /*----------------------------*/
            break                         ;
        /*================================*/
        /* Classe                         */
        /*--------------------------------*/
        case 'C':
            /*----------------------------*/
            /* Focus su classe            */
            /*----------------------------*/
            $("#cod_cla").focus()         ;
            /*----------------------------*/
            /* Oltre                      */
            /*----------------------------*/
            break                         ;
    };

    /*====================================*/
    /* Selezione su Radio buttons         */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $("input[type=radio]").change(function()
    {
        /*--------------------------------*/
        /* Valore bottone selezionato     */
        /*--------------------------------*/
        let sel_val = $(this).val()       ;
        let sel_nom = $(this).attr('name');
        /*--------------------------------*/
        /* Assegnazione al campo          */
        /*--------------------------------*/
        $('#' + sel_nom).val(sel_val)     ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });

    /*====================================*/
    /* Bottoni per grafico               */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $("#grf_spn, #grf_vst, #grf_not, #grf_rsp, #grf_rsq, #grf_ute").click(function()
    {
        /*--------------------------------*/
        /* Tipo grafico                   */
        /*--------------------------------*/
        var tip_grf = $(this).attr("name");        
        /*--------------------------------*/
        /* Titolo dialog                  */
        /*--------------------------------*/
        var tit_grf = "Grafico statistico";        
        /*--------------------------------*/
        /* Parametri da passare           */
        /*--------------------------------*/
        var par_frm = {
            "tip_grf": tip_grf
        };
        /*--------------------------------*/
        /* Apri dialog con iframe         */
        /*--------------------------------*/
        dlg_ifr(1100, 750, tit_grf)       ;
        /*--------------------------------*/
        /* Invia form dinamico            */
        /*--------------------------------*/
        inv_frm(url_grf, par_frm, "ifr_pro");
    });

    /*====================================*/
    /* Bottoni per colore responsabili    */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $("#col_rsp").click(function()
    {
        /*--------------------------------*/
        /* Parametri per il grafico       */
        /*--------------------------------*/
        var tit_grf = "Colori Responsabili";        
        /*--------------------------------*/
        /* Apri dialog con iframe         */
        /*--------------------------------*/
        dlg_ifr(950, 900, tit_grf)        ;
        /*--------------------------------*/
        /* Invia form dinamico            */
        /*--------------------------------*/
        var par_frm = '';
        inv_frm(url_rsp, par_frm, "ifr_pro");
    });

    /*====================================*/
    /* Bottoni per grafico statistico     */
    /*                                    */
    /* Livello '1'                        */
    /*                                    */
    /* ___ SOLO 'mario' ___               */
    /*------------------------------------*/
    $("#grf_svf").click(function()
    {
        /*--------------------------------*/
        /* Parametri per il grafico       */
        /* ___ DA MIGLIORARE ___          */
        /*--------------------------------*/
        var tit_grf = "Statistica SVF200" ;        
     //   var srv = "http://192.168.172.1/";        
        var srv = "http://10.1.10.222/"   ;
        var azi = 'ele'                   ;
        var grf = 'svf200'                ;
        
        /*--------------------------------*/
        /* Utente                         */
        /*--------------------------------*/
        var ute = $('#cod_ute').val()     ;
        /*--------------------------------*/
        /* Crypting MD5                   */
        /*--------------------------------*/
        var md5_e = $.md5(azi)            ;
        var md5_u = $.md5(ute)            ;
        var md5 = md5_e + md5_u           ;
        /*--------------------------------*/
        /* Link per il grafico            */
        /*--------------------------------*/
        var lnk_grf = srv
                    + "chart/t_engine.html?"
                    + "CT=C&EN="
                    + grf
                    + "&ID="
                    + md5
                    + "/"
                    + azi
                    + "_"
                    + grf
                    + ".csv"             ;
        /*--------------------------------*/
        /* Apri dialog con iframe         */
        /*--------------------------------*/
        dlg_ifr(1500, 900, tit_grf)      ;
        /*--------------------------------*/
        /* Invia form dinamico            */
        /*--------------------------------*/
        var par_frm = '';
        inv_frm(lnk_grf, par_frm, "ifr_pro");
    });

    /*====================================*/
    /* Bottone reset                      */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $("#but_res").click(function()
    {
        /*--------------------------------*/
        /* Normalizzazioni                */
        /*--------------------------------*/
        $(':input',mod_frm)
          .not(':button, :submit, :reset, :hidden')
          .val('')
          .prop('checked', false)
          .prop('selected', false)        ;     
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });

    /*====================================*/
    /* Bottone find                       */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $("#but_fnd").click(function()
    {
        /*--------------------------------*/
        /* Normalizzazione prodotto       */
        /*--------------------------------*/
        $("#alf_pro").val('')             ;
        $("#des_pro").val('')             ;
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
            source     : url_fnd,
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
    /* Bottone Logout                     */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $("#log_out").click(function()
    {
        /*================================*/
        /* Rimozione Cookies              */
        /*--------------------------------*/
        Cookies.remove('cod_ute', { path: '/' });
        Cookies.remove('pwd_ute', { path: '/' });
        $('#cod_ute').val("")             ;
        $('#pwd_ute').val("")             ;
        $('#liv_ope').val(0)              ;
        /*--------------------------------*/
        /* Ricarico la pagina             */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });

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
        /* Evidenziazione celle           */
        /*--------------------------------*/
        cel_css("cel_num", "+")           ;
    });

    /*====================================*/
    /* Cambio selettore si/no             */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('.snx').change(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });          

    /*====================================*/
    /* Cambio prodotto                    */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#alf_pro').change(function()
    {
        /*--------------------------------*/
        /* Codice alfanumerico prodotto   */
        /*--------------------------------*/
        var alf_pro = $('#alf_pro').val();
        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm =
        {
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
                  url: "./eleinvP0",
          contentType: "application/x-www-form-urlencoded",
                 data: dat,
              success: function(data)
            {
            /*----------------------------*/
            /* Descrizione prodotto       */
            /*----------------------------*/
            $("#des_pro").val(data)       ;
            }
        });
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });          

    /*====================================*/
    /* Cambio classe, gruppo, sottogruppo */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#cod_cla, #cod_gru, #cod_sgr').change(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()              ;
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
        /* Tipo selezione                 */
        /*--------------------------------*/
        var tip_ord = $("input[name='tip_ord']:checked").val();
        /*--------------------------------*/
        /* Deviazione in funzione del     */
        /* tipo selezione                 */
        /*--------------------------------*/
        switch(tip_ord)
        {
            /*============================*/
            /* Nessuna selezione          */
            /*----------------------------*/
            case '':
                /*------------------------*/
                /* Messaggio              */
                /*------------------------*/
                var msg_err = 'Manca il Tipo ordinamento!';
                dlg_err (msg_err)         ;
                /*------------------------*/
                /* Oltre                  */
                /*------------------------*/
                break                     ;
            /*============================*/
            /* Prodotto                   */
            /*----------------------------*/
            case 'P':
                /*------------------------*/
                /* Test su prodotto       */
                /*------------------------*/
                var alf_pro = $('#alf_pro').val() ;
                /*------------------------*/
                /* Se a spazi             */
                /*------------------------*/
                if (alf_pro == '')
                {
                    /*--------------------*/
                    /* Messaggio          */
                    /*--------------------*/
                    var msg_err = 'Codice Prodotto non selezionato!';
                    dlg_err (msg_err)     ;
                    
                    /*--------------------*/
                    /* Livello successivo */
                    /*--------------------*/
                    $('#liv_ope').val('2');
                    /*--------------------*/
                    /* Caricamento modulo */
                    /*--------------------*/
                    $(mod_frm).submit()   ;
                }
                 else
                {
                    /*--------------------*/
                    /* Livello successivo */
                    /*--------------------*/
                    $('#liv_ope').val('2');
                    /*--------------------*/
                    /* Caricamento modulo */
                    /*--------------------*/
                    $(mod_frm).submit()   ;
                }
                /*------------------------*/
                /* Oltre                  */
                /*------------------------*/
                break                     ;
            /*============================*/
            /* Ubicazione                 */
            /*----------------------------*/
            case 'U':
                /*------------------------*/
                /* Test su ubicazione     */
                /*------------------------*/
                var cod_ubi = $('#cod_ubi').val() ;
                    cod_ubi = cod_ubi.trim()      ;
                    cod_ubi = pul_str (cod_ubi)   ;
                $('#cod_ubi').val(cod_ubi)        ;
                /*------------------------*/
                /* Se a spazi             */
                /*------------------------*/
                if (cod_ubi == '')
                {
                    /*--------------------*/
                    /* Messaggio          */
                    /*--------------------*/
                    var msg_err = 'Manca il codice Ubicazione!';
                    dlg_err (msg_err)     ;
                    /*--------------------*/
                    /* Focus su ubicaz.   */
                    /*--------------------*/
                    $('#cod_ubi').focus() ;
                }
                 else
                {
                    /*--------------------*/
                    /* Livello successivo */
                    /*--------------------*/
                    $('#liv_ope').val('2');
                    /*--------------------*/
                    /* Caricamento modulo */
                    /*--------------------*/
                    $(mod_frm).submit()   ;
                }
                /*------------------------*/
                /* Oltre                  */
                /*------------------------*/
                break                     ;
            /*============================*/
            /* Classe                     */
            /*----------------------------*/
            case 'C':
                /*------------------------*/
                /* Test su ubicazione     */
                /*------------------------*/
                var cod_cla = $('#cod_cla').val() ;
                /*------------------------*/
                /* Se a spazi             */
                /*------------------------*/
                if (cod_cla == 0)
                {
                    /*--------------------*/
                    /* Messaggio          */
                    /*--------------------*/
                    var msg_err = 'Manca il codice Classe!';
                    dlg_err (msg_err)     ;
                }
                 else
                {
                    /*--------------------*/
                    /* Livello successivo */
                    /*--------------------*/
                    $('#liv_ope').val('2');
                    /*--------------------*/
                    /* Caricamento modulo */
                    /*--------------------*/
                    $(mod_frm).submit()   ;
                }
                /*------------------------*/
                /* Oltre                  */
                /*------------------------*/
                break                     ;
            /*============================*/
            /* Nessuna selezione          */
            /*----------------------------*/
            default:
                /*------------------------*/
                /* Messaggio              */
                /*------------------------*/
                var msg_err = 'Manca il Tipo ordinamento!';
                dlg_err (msg_err)         ;
                /*------------------------*/
                /* Oltre                  */
                /*------------------------*/
                break                     ;
        };
        
    /*------------------------------------*/
    /* Bottone conferma - fine            */
    /*====================================*/
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
        $(location).prop('href', '../ele2.html')
    });

/*----------------------------------------*/
/* Fine funzione livello '1'              */
/*========================================*/
}

/*========================================*/
/* Funzione di gestione livello '2'       */
/*                                        */
/* Input Spunte                           */
/*----------------------------------------*/
function liv_002 ()
{

    /*====================================*/
    /* INIBIZIONE TOTALE PROVVISORIA      */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
//    $(":input[type=text]").prop("disabled", true);
//    $(":input[type=number]").prop("disabled", true);
//    $(":input[type=checkbox]").prop("disabled", true);

    /*====================================*/
    /* Definizione tabella                */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    let table = new DataTable('#mxport')  ;

    /*====================================*/
    /* Definizione box di dialogo per     */
    /* conferma spunta totale             */
    /*                                    */
    /* Livello '2'                        */
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
    /* ___ ??? ___                        */
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
    /* Cambio Prodotto da verificare      */
    /*------------------------------------*/

    /*====================================*/
    /* Controllo click su elementi riga   */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    table.on('click', 'td', function ()
    {
        /*--------------------------------*/
        /* Indicatori colonne             */
        /*--------------------------------*/
        var cln_qpr = 11                  ;
        var cln_ubi = cln_qpr + 6         ;
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
        /* Test che sia la colonna della  */
        /* quantita' presunta e non ci    */
        /* sia gia' la spunta             */
        /*--------------------------------*/
        if (num_cln == cln_qpr)
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
        if (num_cln == cln_ubi)
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
    /* Highlight prodotto                 */
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
    /* Modifica box di spunta o quantita' */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    table.on('change', '.chk_spn,.numbersOnly', function ()
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
        var rsp_doc = $('#rsp_doc').val() ;
        var cod_ute = $('#cod_ute').val() ;
        var cod_fas = mod_frm.substring(1);
        var cod_ubi = $(ide_row).find('#ubi').val();
        var alf_pro = $(ide_row).find('#alf').val();
        var num_pro = $(ide_row).find('#pro').val();
        var qta_prs = $(ide_row).find('#qta').val();
        var qta_rlv = $(ide_row).find('#qtv').val();
        /*--------------------------------*/
        /* Campo di spunta                */
        /*--------------------------------*/
        if ($(ide_row).find('.chk_spn').is(":checked"))
//        if ($(ide_row).find('input[type="checkbox"]').is(":checked"))
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
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm =
        {
            "rsp_doc": rsp_doc,
            "cod_ute": cod_ute,
            "cod_fas": cod_fas,
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
            url : url_wrt,
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* DEBUG                  */
                /*------------------------*/
//                console.log(data);
                /*------------------------*/
                /* Ricarica modulo        */
                /*------------------------*/
//                $(mod_frm).submit()    ;
            }
        });
    });          

    /*====================================*/
    /* Modifica box spunta supervisione   */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    table.on('change', '.chk_spv', function ()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WS"                ;
        /*--------------------------------*/
        /* Identificatori riga tabella    */
        /*--------------------------------*/
        var pri_inv = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_inv             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var cod_ute = $('#cod_ute').val() ;
        var cod_fas = mod_frm.substring(1);
        var cod_ubi = $(ide_row).find('#ubi').val();
        var alf_pro = $(ide_row).find('#alf').val();
        var num_pro = $(ide_row).find('#pro').val();
        /*--------------------------------*/
        /* Campo di spunta                */
        /*--------------------------------*/
        if ($(ide_row).find('.chk_spv').is(":checked"))
        {
            /*----------------------------*/
            /* Spunta a 'SI'              */
            /*----------------------------*/
            var flg_spv = 'S'             ;
        }
         else
        {
            /*----------------------------*/
            /* Spunta a 'NO'              */
            /*----------------------------*/
            var flg_spv = 'N'             ;
        }

        /*================================*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm =
        {
            "tip_ope": tip_ope,
            "cod_ute": cod_ute,
            "cod_fas": cod_fas,
            "cod_ubi": cod_ubi,
            "alf_pro": alf_pro,
            "num_pro": num_pro,
            "flg_spv": flg_spv
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
            url : url_wrt,
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
    //            $(mod_frm).submit()     ;
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
//        $('#cod_ubi').val('')             ;
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
    /* Bottone completamento spunte       */
    /*                                    */
    /* Livello '2'                        */
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

/*----------------------------------------*/
/* Fine funzione livello '2'              */
/*========================================*/
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
/* Funzione generica per dialog con       */
/* messaggio di errore                    */
/*----------------------------------------*/
function dlg_err (msg)
{
    /*------------------------------------*/
    /* Titolo messaggio                   */
    /*------------------------------------*/
    var tit = 'ATTENZIONE !'              ;
    /*------------------------------------*/
    /* Composizione messaggio             */
    /*------------------------------------*/
    var err = '<h3><br>' + msg + '</h3>'  ;
    /*------------------------------------*/
    /* Box di dialogo per messaggi        */
    /*------------------------------------*/
    $('#msg_err').html(err)               ;
    /*------------------------------------*/
    /* Box di dialogo per messaggi        */
    /*------------------------------------*/
    $("#msg_err").dialog("option","title",tit).dialog('open');
/*----------------------------------------*/
/* Fine funzione                          */
/*========================================*/
}

/*========================================*/
/* Funzione per creare e gestire un       */
/* Dialog con iframe                      */
/*----------------------------------------*/
function dlg_ifr(wdt_dlg,
                 hgt_dlg,
                 tit_dlg)
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
        title    : tit_dlg,
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


