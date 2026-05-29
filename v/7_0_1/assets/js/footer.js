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
        <a href="https://abcdstudy.org" target="_blank"><i class="fa-solid fa-globe"></i> ABCD Study</a>
        <a href="https://nbdc-datahub.org" target="_blank"><i class="fa-solid fa-globe"></i> NBDC Data Hub</a>
        <a href="https://www.instagram.com/theabcdstudy/" target="_blank"><i class="fa-brands fa-instagram"></i> Instagram</a>
        <a href="https://www.youtube.com/@theabcdstudy" target="_blank"><i class="fa-brands fa-youtube"></i> YouTube</a>
      </div>
    </div>`;
});
