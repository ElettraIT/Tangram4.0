/*========================================*/
/* File : vis_xml.js                      */
/*                                        */
/* Caricamento fatture XML : 13/03/2018   */
/*========================================*/
/*----------------------------------------*/
/* Controllo dei dati                     */
/*----------------------------------------*/
$(document).ready(function()
{






    /*------------------------------------*/
    /* Inibizione iniziale bottoni        */
    /*------------------------------------*/
    $('input[type="button"]').attr('disabled','disabled');
  
  
  
  
    /*------------------------------------*/
    /* Focus su file                      */
    /*------------------------------------*/
    $('#file_jpg').focus()                ;

    /*------------------------------------*/
    /* Test su campo file                 */
    /*------------------------------------*/
    $('#file_jpg').change(function()
    {
        readURL(this);
        
        
        /*--------------------------------*/
        /* Valore campo                   */
        /*--------------------------------*/
        var input=$(this);
        var is_path=input.val();
        /*--------------------------------*/
        /* Se vuoto                       */
        /*--------------------------------*/
        if(!is_path)
        {
            /*----------------------------*/
            /* Messaggio                  */
            /*----------------------------*/
            var text   = "ATTENZIONE: <b>Selezionare la fattura da caricare!</b>";
            $('#info').html(text);
        }
        /*--------------------------------*/
        /* Se scelto                      */
        /*--------------------------------*/
         else
        {
            /*----------------------------*/
            /* Messaggio                  */
            /*----------------------------*/
            var base_path= basename(is_path);
            var ext = this.value.match(/\.(.+)$/)[1];
            if (ext == 'jpg' || ext == 'JPG' || ext == 'jpeg' || ext == 'JPEG')
            {
                $('input[type="button"]').removeAttr('disabled');
            }
             else
            {
                var text   = "FILE NON VALIDO: <b>" + base_path + "</b>";
                $('#info').html(text);
            }
        }
    });    

    /*====================================*/
    /* Bottone di submit                  */
    /*------------------------------------*/
    $("#carica").click(function(){
    
    
        var input=$('#file_jpg');
        var is_path=input.val();
    
    
   //     alert(is_path);

        /*--------------------------------*/
        /* Submit                         */
        /*--------------------------------*/
        $('#img_inp').submit()     ;
        
    });

/*----------------------------------------*/
/* Fine controllo dei dati                */
/*----------------------------------------*/
});

/*========================================*/
/* Funzione basename                      */
/*----------------------------------------*/
function basename(path) {
    return path.replace(/\\/g,'/').replace( /.*\//, '' );
}

function readURL(input) {
  if (input.files && input.files[0]) {
    var reader = new FileReader();
    reader.onload = function(e) {
      $('#previewHolder').attr('src', e.target.result);
    }
    reader.readAsDataURL(input.files[0]);
  } else {
    alert('select a file to see preview');
    $('#previewHolder').attr('src', '');
  }
}
