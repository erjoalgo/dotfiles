// Scripts to inject, as chrome.userScripts.RegisteredUserScript entries.
// Add each script's hosts to host_permissions in manifest.json too.
const SCRIPTS = [
  {
    id: 'edx-close-popup',
    matches: ['https://learning.edx.org/*'],
    js: [{ file: 'edx-close-popup.js' }]
  },
  {
    id: 'realestateu',
    matches: ['https://*.realestateu.com/*'],
    js: [{ file: 'html2pdf.bundle.min.js' }, { file: 'realestateu.js' }]
  },
  {
    id: 'forgejo-expand',
    matches: ['http://localhost:3123/*'],
    js: [{ file: 'forgejo-expand.js' }]
  }
];

async function registerAll() {
  try {
    await chrome.userScripts.unregister();
    await chrome.userScripts.register(SCRIPTS);
  } catch (err) {
    console.error(
      'registering user scripts failed; is "Allow User Scripts" enabled ' +
        "in this extension's details page?",
      err
    );
  }
}

chrome.runtime.onInstalled.addListener(registerAll);
chrome.runtime.onStartup.addListener(registerAll);
