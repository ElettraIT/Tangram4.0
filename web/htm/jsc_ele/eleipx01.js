/*========================================*/
/* eleipx01 - Javascript - N&A            */
/* -------------------------------------- */
/* Funzioni accessorie per determinare il */
/* codice operatore                       */
/* -------------------------------------- */
/* Aggiornamento   : 10/10/2024           */
/* -------------------------------------- */
/* Attualmente utilizzato da elebol01     */
/*                           elepro01     */
/*                           eleubi01     */
/*                           eleubi02     */
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
              url: "./elebfoi0",
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
                  /* Ricarica del modulo  */
                  /*----------------------*/
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


