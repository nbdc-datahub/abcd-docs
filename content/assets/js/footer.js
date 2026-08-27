(function () {
  // Quarto rewrites THIS script's src to a page-relative path (e.g. "./assets/js/footer.js"
  // at the site root, "../../assets/js/footer.js" two levels deep). The <img> srcs below are
  // runtime strings Quarto never sees, so we derive the site root from this script's resolved
  // URL and prefix the asset paths with it. This works at any page depth and under any
  // deployment prefix (local "/", data.abcdstudy.org/reports/website_root/,
  // docs.abcdstudy.org/latest/website_root/, ...). Captured here at parse time because
  // document.currentScript is null once the DOMContentLoaded callback runs.
  const root = document.currentScript
    ? document.currentScript.src.replace(/assets\/js\/footer\.js(\?.*)?(#.*)?$/, '')
    : '';

  document.addEventListener('DOMContentLoaded', function () {
    const footer = document.querySelector('.nav-footer-center');
    if (!footer) return;

    footer.innerHTML = `
    <div class="footer-content">
      <div class="footer-legal">
        <em>ABCD Study</em>®, <em>Teen Brains. Today's Science. Brighter Future.</em>® and the ABCD Study Logo
        are registered marks of the U.S. Department of Health &amp; Human Services (HHS).
        Adolescent Brain Cognitive Development℠ Study is a service mark of the U.S. Department of Health &amp; Human Services (HHS).
      </div>
      <div class="footer-social">
        <a href="https://abcdstudy.org" target="_blank"><img src="${root}assets/img/favicon/apple-touch-icon.png" width="20" height="20" style="filter: opacity(70%)"> ABCD Study</a>
        <a href="https://nbdc-datahub.org" target="_blank"><img src="${root}assets/img/tools/nbdc_icon.png" height="20" style="filter: invert(46%) sepia(11%) saturate(445%) hue-rotate(198deg)
        brightness(92%) contrast(88%)"> NBDC Data Hub</a>
        <a href="https://www.instagram.com/theabcdstudy/" target="_blank"><i class="fa-brands fa-instagram"></i> Instagram</a>
        <a href="https://www.youtube.com/@theabcdstudy" target="_blank"><i class="fa-brands fa-youtube"></i> YouTube</a>
      </div>
    </div>`;
  });
})();
