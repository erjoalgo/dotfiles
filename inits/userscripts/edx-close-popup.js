// Close the "Pursue a verified certificate" popup whenever it appears.
setInterval(() => {
  document
    .querySelector(
      "div[aria-label='Pursue a verified certificate'] button.pgn__modal-close-button"
    )
    ?.click();
}, 500);
