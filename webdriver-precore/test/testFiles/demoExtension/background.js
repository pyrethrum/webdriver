// Background script for BiDiDemoExtension
console.log('BiDiDemoExtension: Background script loaded');

// Listen for extension installation
chrome.runtime.onInstalled.addListener(() => {
  console.log('BiDiDemoExtension: Extension installed');
  
  // Set a simple storage value to indicate the extension is active
  chrome.storage.local.set({
    'bidi_demo_extension_active': true,
    'installation_time': new Date().toISOString()
  });
});

// Listen for action clicks (Manifest V3 uses 'action' instead of 'browserAction')
chrome.action.onClicked.addListener((tab) => {
  console.log('BiDiDemoExtension: Action clicked on tab:', tab.id);
  
  // Send a message to the content script
  chrome.tabs.sendMessage(tab.id, {
    action: 'extensionClicked',
    timestamp: new Date().toISOString()
  });
});

// Listen for messages from content scripts
chrome.runtime.onMessage.addListener((request, sender, sendResponse) => {
  console.log('BiDiDemoExtension: Message received:', request);
  
  if (request.action === 'getExtensionInfo') {
    sendResponse({
      name: 'BiDiDemoExtension',
      version: '1.0.0',
      active: true
    });
  }
  
  return true; // Keep message channel open for async response
});