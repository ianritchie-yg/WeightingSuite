// www/custom.js

$(document).ready(function() {
  // Initialize tooltips
  $('[data-toggle="tooltip"]').tooltip();
  
  // Custom file input behavior
  $('.custom-file-input').on('change', function() {
    let fileName = $(this).val().split('\\').pop();
    $(this).next('.custom-file-label').addClass("selected").html(fileName);
  });
  
  // Sidebar collapse behavior
  $('.sidebar-toggle').on('click', function() {
    setTimeout(function() {
      $(window).trigger('resize'); // Trigger resize for plots
    }, 500);
  });
  
  // Add smooth scrolling
  $(document).on('click', 'a[href^="#"]', function(event) {
    event.preventDefault();
    $('html, body').animate({
      scrollTop: $($.attr(this, 'href')).offset().top
    }, 500);
  });
  
  // Custom plot interaction
  var plotResizeTimer;
  $(window).on('resize', function() {
    clearTimeout(plotResizeTimer);
    plotResizeTimer = setTimeout(function() {
      Shiny.setInputValue('browser_resize', new Date());
    }, 250);
  });
  
  // Enhanced error handling
  $(document).on('shiny:error', function(event) {
    console.error('Shiny Error:', event.error);
    showErrorModal(event.error.message);
  });
  
  // Custom error modal
  function showErrorModal(message) {
    let modalHtml = `
      <div class="modal fade" id="errorModal" tabindex="-1" role="dialog">
        <div class="modal-dialog" role="document">
          <div class="modal-content">
            <div class="modal-header" style="background-color: #004A7E; color: white;">
              <h5 class="modal-title">Error</h5>
              <button type="button" class="close" data-dismiss="modal" style="color: white;">
                <span>&times;</span>
              </button>
            </div>
            <div class="modal-body">
              <p style="color: #333333;">${message}</p>
            </div>
            <div class="modal-footer">
              <button type="button" class="btn btn-secondary" data-dismiss="modal">Close</button>
            </div>
          </div>
        </div>
      </div>
    `;
    
    $('body').append(modalHtml);
    $('#errorModal').modal('show');
    $('#errorModal').on('hidden.bs.modal', function() {
      $(this).remove();
    });
  }
  
  // Custom notifications
  window.showNotification = function(message, type = 'info') {
    let icon = {
      'success': 'check-circle',
      'error': 'exclamation-circle',
      'warning': 'exclamation-triangle',
      'info': 'info-circle'
    }[type];
    
    let colors = {
      'success': '#004A7E',
      'error': '#ED1C24',
      'warning': '#666666',
      'info': '#004A7E'
    };
    
    let notification = `
      <div class="custom-notification ${type}" style="background-color: ${colors[type]}; color: white;">
        <i class="fas fa-${icon}"></i>
        <span>${message}</span>
      </div>
    `;
    
    let $notification = $(notification).appendTo('body');
    setTimeout(() => {
      $notification.fadeOut(() => $notification.remove());
    }, 3000);
  };
  
  // Performance monitoring
  let performanceLog = [];
  
  window.logPerformance = function(action, duration) {
    performanceLog.push({
      action: action,
      duration: duration,
      timestamp: new Date()
    });
    
    if (performanceLog.length > 100) {
      performanceLog.shift();
    }
    
    Shiny.setInputValue('performance_log', performanceLog);
  };
  
  // Real-time validation feedback
  $(document).on('change', '.target-input', function() {
    var input = $(this);
    var values = input.val().split(',').map(Number);
    var sum = values.reduce((a, b) => a + b, 0);
    
    if (Math.abs(sum - 1) > 0.01) {
      input.addClass('has-error');
      input.next('.help-block').text('Sum should be close to 1 (current: ' + sum.toFixed(3) + ')');
    } else {
      input.removeClass('has-error');
      input.next('.help-block').text('');
    }
  });

  // Theme switching functionality
  window.switchTheme = function(themeName) {
    // Remove all theme classes
    document.body.classList.remove(
      'grapefruit-theme',
      'plum-theme',
      'blueberry-theme',
      'avocado-theme',
      'pomegranate-theme'
    );
    
    // Add new theme class
    document.body.classList.add('fruit-theme', `${themeName}-theme`);
    
    // Update active state of theme options
    $('.theme-option').removeClass('active');
    $(`.theme-option[data-theme="${themeName}"]`).addClass('active');
    
    // Store theme preference
    localStorage.setItem('preferred-theme', themeName);
    
    // Notify Shiny of theme change
    Shiny.setInputValue('theme_changed', themeName);
  };

  // Initialize theme from stored preference
  const storedTheme = localStorage.getItem('preferred-theme') || 'grapefruit';
  window.switchTheme(storedTheme);
});

function switchTheme(themeName) {
  // Remove active class from all buttons
  document.querySelectorAll('.theme-option').forEach(button => {
    button.classList.remove('active');
  });
  
  // Add active class to selected theme button
  document.querySelector(`[data-theme="${themeName}"]`).classList.add('active');
  
  // Notify Shiny of theme change
  Shiny.setInputValue('theme_changed', themeName);
}