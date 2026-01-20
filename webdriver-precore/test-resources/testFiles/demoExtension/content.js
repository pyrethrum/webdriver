// Content script for BiDiDemoExtension
console.log('BiDiDemoExtension: Content script loaded on:', window.location.href);

// Add a visible indicator to the page
function addExtensionIndicator() {
  // Check if indicator already exists
  if (document.getElementById('bidi-demo-extension-indicator')) {
    return;
  }
  
  const indicator = document.createElement('div');
  indicator.id = 'bidi-demo-extension-indicator';
  indicator.style.cssText = `
    position: fixed;
    top: 10px;
    right: 10px;
    background: #4CAF50;
    color: white;
    padding: 8px 12px;
    border-radius: 4px;
    font-family: Arial, sans-serif;
    font-size: 12px;
    z-index: 999999;
    box-shadow: 0 2px 8px rgba(0,0,0,0.3);
  `;
  indicator.textContent = 'BiDiDemoExtension Active';
  
  document.body.appendChild(indicator);
  
  // Store in window object for easy WebDriver access
  window.biDiDemoExtension = {
    active: true,
    name: 'BiDiDemoExtension',
    version: '1.0.0',
    indicatorElement: indicator
  };
}

// Add indicator when DOM is ready
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', addExtensionIndicator);
} else {
  addExtensionIndicator();
}

// Listen for messages from background script
chrome.runtime.onMessage.addListener((request, sender, sendResponse) => {
  console.log('BiDiDemoExtension: Content script received message:', request);
  
  if (request.action === 'extensionClicked') {
    // Flash the indicator to show interaction
    const indicator = document.getElementById('bidi-demo-extension-indicator');
    if (indicator) {
      indicator.style.background = '#FF9800';
      setTimeout(() => {
        indicator.style.background = '#4CAF50';
      }, 500);
    }
    
    sendResponse({ success: true });
  }
  
  return true;
});

// Expose a global function for WebDriver testing
window.getBiDiDemoExtensionStatus = function() {
  return {
    active: true,
    name: 'BiDiDemoExtension',
    hasIndicator: !!document.getElementById('bidi-demo-extension-indicator'),
    url: window.location.href,
    timestamp: new Date().toISOString()
  };
};