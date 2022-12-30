$(".layer-button").hover(function() { 
    $( this ).attr('Title', 'Layers'); 
    $( this ).css('color', 'blue');
  }, function() {
    $( this ).css('color', 'black');
  });