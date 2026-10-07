document.addEventListener('DOMContentLoaded', function() {
  // Use querySelectorAll to get all search buttons
  const searchButtons = document.querySelectorAll('.sidebar-search');
  
  searchButtons.forEach(searchButton => {
    searchButton.addEventListener('click', function() {
      const navbarSearchButton = document.querySelector('#quarto-search .aa-DetachedSearchButton');
      if (navbarSearchButton) {
        navbarSearchButton.click();
      }
    });
  });
});