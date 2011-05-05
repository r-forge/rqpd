
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  <style type="text/css">
    #main {
      margin-left: 20px;
      width: 800px;
    }
  </style>
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php /* 
if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } 
 */?>

<!-- end of project description -->

<div id="main">
<h1>R package rqpd: Regression Quantiles for Panel Data</h1>
<p>With rqpd you can fit fixed-effects [1] and correlated-random-effects quantile regression models [2,3] and do (bootstrap) inference.</p>

<p>You can install the package by typing 'install.packages("rqpd", repos="http://R-Forge.R-project.org")' in R.</p>

<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

<h2>References:</h2>
<ol>
  <li>
  Koenker, Roger. 2004. Quantile regression for longitudinal data. 
  Journal of Multivariate Analysis. 91-1. p. 74--89. 
  </li>
  <li>
  Abrevaya, Jason and Christian M. Dahl. 2008.
  The effects of birth inputs on birthweight.
  Journal of Business and Economic Statistics. 26-4. p. 379--397.
  </li>
  <li>
  Bache, Stefan Holst; Christian M. Dahl and Johannes Tang Kristensen. 2011.
  Headlights on tobacco road to low birthweight outcomes --
  Evidence from a batery of quantile regression estimators and a 
  heterogenous panel. <a href="http://econ.au.dk/fileadmin/site_files/filer_oekonomi/Working_Papers/CREATES/2008/rp08_20.pdf">Working paper.</a>
  </li> 
</ol>
</div>


</body>
</html>
