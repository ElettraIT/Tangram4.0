/*========================================*/
/* eleipx01 - Javascript - N&A            */
/* -------------------------------------- */
/* Funzioni accessorie per determinare il */
/* codice operatore                       */
/* -------------------------------------- */
/* Aggiornamento   : 10/10/2024           */
/* -------------------------------------- */
/* Attualmente utilizzato da elebfoB1     */
/*                           elebfoB2     */
/*                           eleodsB1     */
/*                           eleabb01     */
/*                           eleabb02     */
/*========================================*/

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
              url: "../cgi-bin/elebfoi0",
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
                  /* Descrizione operatore*/
                  /*----------------------*/
                  var des = $('#rsp_doc option:selected').text();
                  $('#rsp_des').val((des.trim()));
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
