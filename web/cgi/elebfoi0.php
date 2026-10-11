<?php
//set random name for the image, used time() for uniqueness
$filename =  time() . '.jpg';
$filepath = 'saved_images/';
move_uploaded_file($_FILES['webcam']['tmp_name'], $filepath.$filename);

echo "Content-type: text/html\n\n"   ;


echo $filepath.$filename;
?>