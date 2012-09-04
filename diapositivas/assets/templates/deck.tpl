<!DOCTYPE html>
<!--[if lt IE 7]> <html class="no-js ie6" lang="en"> <![endif]-->
<!--[if IE 7]>    <html class="no-js ie7" lang="en"> <![endif]-->
<!--[if IE 8]>    <html class="no-js ie8" lang="en"> <![endif]-->
<!--[if gt IE 8]><!-->  <html class="no-js" lang="en"> <!--<![endif]-->
<head>
	<meta charset="utf-8">
	<meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1">
	<meta name="viewport" content="width=1024, user-scalable=no">
	
	<title>{{ title }}</title>
  <meta name="description" content="{{title}}">
  <meta name="author" content="{{author}}">
  <meta name="generator" content="slidify" />
	
	
	
	<!-- Core and extension CSS files -->
	<link rel="stylesheet" href="{{lib_path}}/deck.js/core/deck.core.css">
	<link rel="stylesheet" href="{{lib_path}}/deck.js/extensions/goto/deck.goto.css">
	<link rel="stylesheet" href="{{lib_path}}/deck.js/extensions/menu/deck.menu.css">
	<link rel="stylesheet" href="{{lib_path}}/deck.js/extensions/navigation/deck.navigation.css">
	<link rel="stylesheet" href="{{lib_path}}/deck.js/extensions/status/deck.status.css">
	<link rel="stylesheet" href="{{lib_path}}/deck.js/extensions/hash/deck.hash.css">
	<link rel="stylesheet" href="{{lib_path}}/deck.js/extensions/scale/deck.scale.css">
	<link rel="stylesheet" href="{{lib_path}}/deck.js/extensions/codemirror/deck.codemirror.css">
	<link rel="stylesheet" href="{{lib_path}}/deck.js/extensions/codemirror/themes/default.css">
	<!-- Style theme. More available in /themes/style/ or create your own. -->
	<link rel="stylesheet" href="{{lib_path}}/deck.js/themes/style/{{theme}}.css">
	<!-- Transition theme. More available in /themes/transition/ or create your own. -->
	<link rel="stylesheet" href="{{lib_path}}/deck.js/themes/transition/{{transition}}.css">
	<script src="{{lib_path}}/deck.js/support/modernizr.custom.js"></script>
	
  <!-- LOAD STYLE SHEETS -->
	<link rel="stylesheet" href="{{lib_path}}/{{highlighter}}/styles/{{histyle}}.css">
  {{> user_css}}
  
</head>

<body class="deck-container">

  <!-- Begin slides -->
  {{#slides}}
    {{{ slide }}}
  {{/slides}}
  <!-- deck.navigation snippet -->
  <a href="#" class="deck-prev-link" title="Previous">&#8592;</a>
  <a href="#" class="deck-next-link" title="Next">&#8594;</a>

  <!-- deck.status snippet -->
  <p class="deck-status">
	<span class="deck-status-current"></span>
	  /
	<span class="deck-status-total"></span>
  </p>

  <!-- deck.goto snippet -->
  <form action="." method="get" class="goto-form">
  	<label for="goto-slide">Go to slide:</label>
  	<input type="text" name="slidenum" id="goto-slide" list="goto-datalist">
  	<datalist id="goto-datalist"></datalist>
  	<input type="submit" value="Go">
  </form>

  <!-- deck.hash snippet -->
  <a href="." title="Permalink to this slide" class="deck-permalink">#</a>


  <!-- Grab CDN jQuery, with a protocol relative URL; fall back to local if offline -->
  <script src="//ajax.aspnetcdn.com/ajax/jQuery/jquery-1.7.min.js"></script>
  <script>window.jQuery || document.write('<script src="{{lib_path}}/deck.js/support/jquery-1.7.min.js"><\/script>')</script>

  <!-- Deck Core and extensions -->
  <script src="{{lib_path}}/deck.js/core/deck.core.js"></script>
  <script src="{{lib_path}}/deck.js/extensions/hash/deck.hash.js"></script>
  <script src="{{lib_path}}/deck.js/extensions/menu/deck.menu.js"></script>
  <script src="{{lib_path}}/deck.js/extensions/goto/deck.goto.js"></script>
  <script src="{{lib_path}}/deck.js/extensions/status/deck.status.js"></script>
  <script src="{{lib_path}}/deck.js/extensions/navigation/deck.navigation.js"></script>
  <script src="{{lib_path}}/deck.js/extensions/scale/deck.scale.js"></script>
  <script src="{{lib_path}}/deck.js/extensions/codemirror/codemirror.js"></script>
  <script src="{{lib_path}}/deck.js/extensions/codemirror/mode/javascript/javascript.js"></script>
  <script src="{{lib_path}}/deck.js/extensions/codemirror/deck.codemirror.js"></script>


  <!-- Initialize the deck -->
  <script>
  $(function() {
  	$.deck('.slide');
  });
  </script>
  
  {{> highlight_js}}
  {{> mathjax}}
  {{> user_js}}

</body>
</html>
