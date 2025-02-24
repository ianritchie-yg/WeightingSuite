
    $(document).ready(function() {
      // Theme switching logic
      $('.theme-switcher select').change(function() {
        var theme = $(this).val();
        $('#main-content').attr('class', 'fruit-theme ' + theme + '-theme');
      });
    });
    
