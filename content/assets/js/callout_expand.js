function expandCalloutById(id) {
  const element = document.getElementById(id);
  
  if (!element || !element.classList.contains('callout')) return false;
  if (!element.firstElementChild || element.firstElementChild.getAttribute('aria-expanded') === 'true') return false;
  
  element.firstElementChild.click();
  
  setTimeout(() => {
    const elementTop = element.getBoundingClientRect().top + window.pageYOffset;
    window.scrollTo({
      top: elementTop - 88, // Offset for navbar
      behavior: 'smooth'
    });
  }, 100);
  
  return true;
}

// Handle hash navigation and changes
function handleHashNavigation() {
  const hash = window.location.hash.substring(1);
  if (hash) {
    expandCalloutById(hash);
  }
}

// Listen for page load and hash changes
document.addEventListener('DOMContentLoaded', () => {
  setTimeout(handleHashNavigation, 100);
});

window.addEventListener('hashchange', handleHashNavigation);

// Handle clicks on internal links with hashtags
document.addEventListener('click', (event) => {
  const link = event.target.closest('a');
  if (!link) return;
  
  const href = link.getAttribute('href');
  if (!href) return;
  
  let targetId = null;
  
  // Direct hash link (#section)
  if (href.startsWith('#')) {
    targetId = href.substring(1);
  }
  // Same-page link with hash (page.html#section)
  else if (href.includes('#') && !href.startsWith('http')) {
    const hashIndex = href.indexOf('#');
    const linkPath = href.substring(0, hashIndex);
    const currentPath = window.location.pathname;
    
    // Only handle if it's the current page
    if (!linkPath || linkPath === currentPath || linkPath === window.location.href.split('#')[0]) {
      targetId = href.substring(hashIndex + 1);
    }
  }
  
  if (targetId) {
    setTimeout(() => expandCalloutById(targetId), 50);
  }
});