// Click through RealEstateU lessons, saving each transcript as a PDF
// (via html2pdf.bundle.min.js, fetched from vendor.txt) before moving on.

const downloaded = new Set();

async function savePDF(filename, element) {
  if (downloaded.has(filename)) {
    console.log('skipping re-download');
    return;
  }
  await html2pdf().set({ filename }).from(element).save();
  console.log(`adding ${filename} to downloads...`);
  downloaded.add(filename);
}

async function saveTranscript() {
  const title = document.querySelector('.page-header-lesson').innerText;
  const filename = title.replace(/[^a-zA-Z0-9]/g, '-') + '.pdf';
  savePDF(filename, document.querySelector('div.transcript'));
}

function clickNext() {
  console.log('attempting to click next...');
  const button = document.querySelector('button.next');
  console.log(`button: ${button}`);
  if (button) {
    saveTranscript();
    button.click();
  }
}

if (confirm('Auto click next?')) {
  setInterval(clickNext, 1000);
} else {
  console.log('skipping auto-click');
}
