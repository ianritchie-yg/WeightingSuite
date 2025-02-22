# Code Attributions

This document tracks third-party code usage in the WeightingSuite project.

## Modal Dialog HTML Structure
**Source:** https://github.com/fedora-infra/bodhi
**License:** GPL-2.0
**Commit:** 50bb5bb35686d439d706f46e795ff0bc944335cd
**Retrieved:** February 2024
**Used in:** custom.js - showErrorModal function
**Modifications:** Adapted styling to match YouGov brand guidelines

```html
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
```

## Notification System
**Source:** https://github.com/mydatahack/serverless-webapp-s3uploader
**Commit:** 01ccc1100de582e933eeadf2af753283dcb9d493
**Retrieved:** February 2024
**Used in:** custom.js - showNotification function
**License Status:** Pending (Repository owner contacted for clarification)
**Modifications:** 
- Adapted styling to YouGov brand colors
- Added icon support
- Modified animation timing

## Note on Licensing
Some components are pending license clarification. We are actively working to:
1. Contact repository owners for license information
2. Document all usage permissions
3. Ensure compliance with all third-party licenses

## Updates
This document will be updated as additional attributions are added or licensing information is clarified.
