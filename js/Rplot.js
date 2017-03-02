(function($) {
    $(document).ready(function() {
	
	$('#Rplot').scianimator({
	    'images': ['images/Rplot1.png', 'images/Rplot2.png', 'images/Rplot3.png', 'images/Rplot4.png', 'images/Rplot5.png', 'images/Rplot6.png', 'images/Rplot7.png', 'images/Rplot8.png'],
	    'width': 480,
	    'delay': 10,
	    'loopMode': 'none',
 'controls': ['first', 'previous', 'play', 'next', 'last', 'loop', 'speed'], 'delayMin': 0
	});
    });
})(jQuery);
