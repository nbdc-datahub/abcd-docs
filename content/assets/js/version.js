// Credits: https://github.com/Deltares/Delft-FIAT/blob/master/docs/_static/version.js
// Modified from above by Le Zhang
function checkPathExists(url) {
    return new Promise((resolve, reject) => {
        const xhr = new XMLHttpRequest();
        xhr.open('HEAD', url, true);
        xhr.onreadystatechange = function() {
            if (xhr.readyState === 4) {
                if (xhr.status === 200) {
                    resolve(true);
                } else if (xhr.status === 404) {
                    resolve(false);
                } else {
                    reject(new Error(xhr.statusText));
                }
            }
        };
        xhr.onerror = function() {
            reject(new Error('Network Error'));
        };
        xhr.send();
    });
}
//set up
const currentLocation = window.location;
if (currentLocation.hostname.includes("data.abcdstudy.org")) {
  document.querySelector('#nav-menu-version').style.display = 'none';
  // exit the script immediately if hostname matches
  throw new Error('Script terminated due to matching hostname.');
}

import vPathRaw from '../../v.json' with { type: "json" };

if (typeof vPathRaw === 'undefined' || !Array.isArray(vPathRaw)) {
  throw new Error("vPath was not imported or is not an array");
}
// version sort, higher on top
const vPath = (f=>f(f(vPathRaw,1).sort(),-1)) ((arr,v)=>arr.map(a=>a.replace(/\d+/g,n=>+n+v*100000))).reverse();
const vText = vPath.map(item => {
  return item.replace(/\_/g, '.');
});
window.onload = function() {

  var currentPagePath = currentLocation.pathname;

  // add some style for the dropdown
  const style = document.createElement('style');
  style.textContent = `
  .dropdown-item.active-item {
    font-weight: bold;
    text-decoration: underline;
  }
  `;
  document.body.appendChild(style);

  // add version dropdown
  const dropdown = document.querySelector('#nav-menu-version').nextElementSibling;
  dropdown.innerHTML = ''; 
  
  const prefixRegex = /^(\/[^\/]+)?(\/(latest|v\/[^\/]+))/;
  const locationMatch = currentLocation.pathname.match(prefixRegex);
  const additionalPath = locationMatch && locationMatch[1] ? locationMatch[1] : '';
  
  for (let i = 0; i < vPath.length; i++) {
    const li = document.createElement('li');
    const a = document.createElement('a');
    a.className = 'dropdown-item';
    a.href = i !== 0 ? `${additionalPath}/v/${vPath[i]}/`: `${additionalPath}/latest/`;
    a.textContent = i !== 0 ? vText[i] : vText[i] + ' (latest)';
    li.appendChild(a);
    dropdown.appendChild(li);
  }
  
  const windowPath = currentLocation.pathname;
  const dropdownItems = dropdown.querySelectorAll('.dropdown-item');
  for (var i = 0; i < dropdownItems.length; i++) {
    const href = dropdownItems[i].getAttribute('href');
    const textContent = dropdownItems[i].textContent;
    // add active-item class to the current version
    if (windowPath.includes(href)) {
      dropdownItems[i].classList.add('active-item');
    } else {
      dropdownItems[i].classList.remove('active-item');
    }
    // set up redirect on click
    dropdownItems[i].addEventListener('click', function(event) {
      event.preventDefault();
      const href = this.getAttribute('href'); 
      // Remove the version part from the current path and extract the extended path
      const regex = new RegExp(`^${additionalPath}(?:\\/(?:v\\/[^\\/]+|latest))(\\/.*)?$`);
      const match = currentPagePath.match(regex);
      const extraPath = (match && match[1]) ? match[1] : '';
      // Build the new URL by combining the clicked version's base href and the extended path
      const newUrl = href.replace(/\/$/, '') + extraPath;
      checkPathExists(newUrl)
        .then(exists => {
          if (!exists) {
            const popup = document.createElement('div');
            popup.textContent = `"${extraPath}" is not found in ${textContent}, going to homepage of ${textContent}.`;
            popup.style.position = 'fixed';
            popup.style.top = '50%';
            popup.style.left = '50%';
            popup.style.transform = 'translate(-50%, -50%)';
            popup.style.padding = '10px 20px';
            popup.style.backgroundColor = '#333';
            popup.style.color = '#fff';
            popup.style.borderRadius = '5px';
            popup.style.boxShadow = '0 2px 6px rgba(0, 0, 0, 0.3)';
            popup.style.zIndex = '1000';
            document.body.appendChild(popup);
            setTimeout(() => {
              popup.remove();
              window.location.href = href;
            }, 2000);
          } else {
            window.location.href = newUrl;
          }
        })
        .catch(() => {
          window.location.href = href;
        });
    });
  }
};
  