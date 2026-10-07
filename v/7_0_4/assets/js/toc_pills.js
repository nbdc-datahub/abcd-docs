document.addEventListener("DOMContentLoaded", () => {
  document.querySelectorAll("#TOC .pill-tabulated, #TOC .pill-file-based, #TOC .pill-concatenated, #TOC .pill-uncategorized").forEach(el => el.remove());
});
