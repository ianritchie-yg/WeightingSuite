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
            <div class="modal-header">
              <h5 class="modal-title">Error</h5>
              <button type="button" class="close" data-dismiss="modal">
                <span>&times;</span>
              </button>
            </div>
            <div class="modal-body">
              <p>${message}</p>
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
    
    let notification = `
      <div class="custom-notification ${type}">
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
});