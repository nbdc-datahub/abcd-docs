window.addEventListener("load", function() {
  // Get all section containers at all levels
  const allSections = document.querySelectorAll("#quarto-sidebar li.sidebar-item-section");
  
  allSections.forEach(section => {
    // Find the immediate child ul (the direct nested list)
    const immediateChildUl = section.querySelector(":scope > ul");
    if (!immediateChildUl) return;
    
    // Check if this ul contains only one nested sidebar-item-section
    const nestedSections = immediateChildUl.querySelectorAll(":scope > li.sidebar-item-section");
    
    if (nestedSections.length === 1) {
      const nestedSection = nestedSections[0];
      
      // Find the deepest nested list within this section
      const allNestedLists = nestedSection.querySelectorAll("ul");
      if (allNestedLists.length > 0) {
        // Find the deepest list by depth class
        const depthNumbers = Array.from(allNestedLists).map(list => {
          const depthClass = list.className.match(/depth\d+/);
          return depthClass ? parseInt(depthClass[0].replace('depth', '')) : 0;
        });
        
        const maxDepth = Math.max(...depthNumbers);
        const maxDepthIdx = depthNumbers.indexOf(maxDepth);
        const deepestList = allNestedLists[maxDepthIdx];
        
        // Move all items from the deepest list to the immediate parent ul
        if (deepestList && deepestList.children.length > 0) {
          Array.from(deepestList.children).forEach(item => {
            immediateChildUl.appendChild(item);
          });
          
          // Remove the nested section wrapper
          nestedSection.remove();
        }
      }
    }
  });
});
