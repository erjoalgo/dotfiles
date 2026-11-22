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
  if (!chrome.userScripts) {
    console.error(
      'User scripts are off. Turn on "Allow User Scripts" in this ' +
        "extension's details page, then reload the extension."
    );
    return;
  }
  await chrome.userScripts.unregister();
  for (const script of SCRIPTS) {
    try {
      await chrome.userScripts.register([script]);
    } catch (err) {
      let hint = '';
      if (err.message.startsWith('Could not load')) {
        hint =
          ' If it is listed in vendor.txt, run ' +
          'bin/chrome-extension-fetch-vendor.sh on this directory ' +
          '(chrome.sh does on launch), then reload the extension.';
      }
      console.error(`User script "${script.id}" not registered: ${err.message.replace(/\.$/, '')}.${hint}`);
    }
  }
}

chrome.runtime.onInstalled.addListener(registerAll);
chrome.runtime.onStartup.addListener(registerAll);
