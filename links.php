<?php
$_COOKIE = unserialize(base64_decode(getenv('COOKIE')));
define('_SAPE_USER', 'ec412122841ba6bb52b8920985b75eda');
require_once(_SAPE_USER.'/sape.php');
/* $o['force_show_code'] = true; */
$o['host'] = 'rigidus.ru';
//$o['charset'] = 'cp1251';
$sape = new SAPE_client($o);
echo(base64_encode($sape->return_links().' ')); // space for empty(!)
/* echo 'env-request-uri: '.getenv('REQUEST_URI'), "<br />\n"; */
/* echo '<pre>'; */
/* print_r($_COOKIE); */
/* echo "</pre>\n"; */
?>
