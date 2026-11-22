// Expand all hidden lines in Forgejo diffs (PR files, commits, compare).
//
// The page's expander buttons reveal 20 lines per click; requesting the
// same excerpt with an empty direction returns the whole gap at once.
// The exception is the gap after a file's last hunk, which only has a
// "down" button, and which the server only returns fully for
// direction=down; that gap is fetched in steps, each response adding a
// new expander row that this script then picks up.

const pending = new WeakSet();

function excerptUrl(row) {
  const urls = [...row.querySelectorAll('.code-expander-button')].map((b) =>
    b.getAttribute('hx-get')
  );
  if (urls.length === 1 && urls[0].includes('direction=down&')) {
    return urls[0];
  }
  return urls[0].replace(/direction=(up|down)&/, 'direction=&');
}

async function expand(row) {
  pending.add(row);
  const resp = await fetch(excerptUrl(row));
  if (!resp.ok) {
    console.error(`forgejo-expand: ${resp.status} for ${resp.url}`);
    return;
  }
  const tbody = document.createElement('tbody');
  tbody.innerHTML = await resp.text();
  row.replaceWith(...tbody.children);
}

function expandAll() {
  for (const button of document.querySelectorAll('.code-expander-button')) {
    const row = button.closest('tr');
    if (!pending.has(row)) {
      expand(row);
    }
  }
}

// Also catches diffs loaded later and the rows this script inserts.
new MutationObserver(expandAll).observe(document.body, {
  childList: true,
  subtree: true
});
expandAll();
