// Popup script for BiDiDemoExtension
console.log('BiDiDemoExtension: Popup script loaded');

document.addEventListener('DOMContentLoaded', function() {
  const testBtn = document.getElementById('testBtn');
  const infoBtn = document.getElementById('infoBtn');
  
  // Test button functionality
  testBtn.addEventListener('click', function() {
    console.log('BiDiDemoExtension: Test button clicked');
    
    // Get the current active tab
    chrome.tabs.query({ active: true, currentWindow: true }, function(tabs) {
      if (tabs[0]) {
        // Send a test message to the content script
        chrome.tabs.sendMessage(tabs[0].id, {
          action: 'extensionClicked',
          source: 'popup',
          timestamp: new Date().toISOString()
        }, function(response) {
          console.log('BiDiDemoExtension: Test response:', response);
        });
        
        // Store test interaction in storage
        chrome.storage.local.set({
          'last_test_click': new Date().toISOString(),
          'test_count': Date.now()
        });
      }
    });
    
    // Close popup after action
    window.close();
  });
  
  // Info button functionality
  infoBtn.addEventListener('click', function() {
    console.log('BiDiDemoExtension: Info button clicked');
    
    // Get extension info from storage
    chrome.storage.local.get(['bidi_demo_extension_active', 'installation_time', 'last_test_click'], function(data) {
      const info = {
        name: 'BiDiDemoExtension',
        version: '1.0.0',
        active: data.bidi_demo_extension_active || false,
        installed: data.installation_time || 'unknown',
        lastTest: data.last_test_click || 'never'
      };
      
      alert('Extension Info:\n' + JSON.stringify(info, null, 2));
    });
  });
  
  // Update UI with current status
  chrome.storage.local.get(['installation_time'], function(data) {
    if (data.installation_time) {
      const installDate = new Date(data.installation_time).toLocaleDateString();
      const infoDiv = document.querySelector('.info');
      if (infoDiv) {
        infoDiv.textContent = `Installed: ${installDate}`;
      }
    }
  });
});