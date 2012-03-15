<?php
// http://www.php.ru/manual/features.commandline.html
// http://help.sape.ru/sape/faq/270
/* echo 'php-current-dir: '.getcwd(), "<br />\n"; */
/* echo 'document-root: '.$_SERVER['DOCUMENT_ROOT'], "<br />\n"; */
/* echo 'env-request-uri: '.getenv('REQUEST_URI'), "<br />\n"; */
/* echo 'stdin: '.file_get_contents('php://stdin') , "<br />\n"; */
$_COOKIE = unserialize(base64_decode(getenv('COOKIE')));
define('_SAPE_USER', 'ec412122841ba6bb52b8920985b75eda');
require_once(_SAPE_USER.'/sape.php');
/* $o['force_show_code'] = true; */
$o['host'] = 'rigidus.ru';
//$o['charset'] = 'cp1252';
/* $o['debug'] = true; */
$sape_context = new SAPE_context($o);

$content = file_get_contents('php://stdin');

//$fp = fopen('data.txt', 'w');
//fwrite($fp, $content);
//fclose($fp);

echo(base64_encode(
  //$sape_context->replace_in_text_segment(file_get_contents('php://stdin'))
  //base64_decode(file_get_contents('php://stdin'))
  //base64_decode(file_get_contents('php://stdin').' ') // space for empty(!)
  $content
));
?>