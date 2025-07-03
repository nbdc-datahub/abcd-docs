/**
 * Dynamic URL handler for multi-environment deployments
 * Automatically adjusts paths based on the current deployment context
 */

class URLHandler {
    constructor(debug = false) {
        this.debug = debug;
        this.baseUrl = this.detectBaseUrl();
        this.log(`Detected base URL: ${this.baseUrl}`);
        this.init();
    }

    log(message) {
        if (this.debug) {
            console.log(`[URLHandler] ${message}`);
        }
    }

    detectBaseUrl() {
        const hostname = window.location.hostname;
        const pathname = window.location.pathname;
        
        this.log(`Detecting base URL for hostname: ${hostname}, pathname: ${pathname}`);
        
        // Local development - root path
        if (hostname === 'localhost' || hostname === '127.0.0.1') {
            this.log('Detected local development environment');
            return '';
        }
        
        // Latest version deployment
        if (pathname.startsWith('/latest/')) {
            this.log('Detected latest version deployment');
            return '/latest';
        }
        
        // Versioned deployments: /reports/.*/ or /v/.*/
        const versionMatch = pathname.match(/^\/(?:reports|v)\/([^\/]+)/);
        if (versionMatch) {
            const prefix = pathname.match(/^\/reports\//) ? 'reports' : 'v';
            const baseUrl = `/${prefix}/${versionMatch[1]}`;
            this.log(`Detected versioned deployment: ${baseUrl}`);
            return baseUrl;
        }
        
        // Default production (root)
        this.log('Using default production (root) deployment');
        return '';
    }

    // Convert absolute paths to relative paths based on current context
    adjustPath(path) {
        if (!path.startsWith('/')) {
            return path; // Already relative
        }
        
        if (this.baseUrl === '') {
            return path; // Local development or root deployment
        }
        
        // Check if the path already has the base URL to avoid double-prefixing
        if (path.startsWith(this.baseUrl + '/')) {
            return path; // Already adjusted
        }
        
        return this.baseUrl + path;
    }

    // Fix all links and images on the page
    fixPageUrls() {
        // Fix all absolute links
        document.querySelectorAll('a[href^="/"]').forEach(link => {
            const href = link.getAttribute('href');
            const adjustedHref = this.adjustPath(href);
            if (href !== adjustedHref) {
                // Mark the link as processed to avoid re-processing
                link.setAttribute('data-url-processed', 'true');
                link.setAttribute('href', adjustedHref);
            }
        });

        // Special handling for reactable tables
        this.fixReactableUrls();
    }

    // Special method to handle reactable content
    fixReactableUrls() {
        // Reactable tables might have different structure, so we check for common reactable classes
        const reactables = document.querySelectorAll('.reactable, [class*="reactable"]');
        
        reactables.forEach(table => {
            // Fix links within reactable tables that haven't been processed yet
            table.querySelectorAll('a[href^="/"]').forEach(link => {
                // Skip if already processed
                if (link.hasAttribute('data-url-processed')) {
                    return;
                }
                
                const href = link.getAttribute('href');
                const adjustedHref = this.adjustPath(href);
                if (href !== adjustedHref) {
                    link.setAttribute('data-url-processed', 'true');
                    link.setAttribute('href', adjustedHref);
                }
            });
        });
    }

    init() {
        // Run when DOM is ready
        if (document.readyState === 'loading') {
            document.addEventListener('DOMContentLoaded', () => this.fixPageUrls());
        } else {
            this.fixPageUrls();
        }

        // Event delegation for click events on links (handles dynamically loaded content)
        document.addEventListener('click', (event) => {
            const link = event.target.closest('a[href^="/"]');
            if (link) {
                const href = link.getAttribute('href');
                const adjustedHref = this.adjustPath(href);
                
                this.log(`Click intercepted: ${href} -> ${adjustedHref}`);
                
                // Only update if the path actually changed
                if (href !== adjustedHref) {
                    event.preventDefault();
                    this.log(`Redirecting to: ${adjustedHref}`);
                    
                    // Check if it's an external link or should open in new tab
                    if (link.target === '_blank' || event.ctrlKey || event.metaKey) {
                        window.open(adjustedHref, '_blank');
                    } else {
                        window.location.href = adjustedHref;
                    }
                }
            }
        });

        // Also run whenever new content is added (for immediate visual updates)
        const observer = new MutationObserver((mutations) => {
            let shouldUpdate = false;
            mutations.forEach(mutation => {
                if (mutation.type === 'childList' && mutation.addedNodes.length > 0) {
                    // Check if any added nodes contain unprocessed links
                    mutation.addedNodes.forEach(node => {
                        if (node.nodeType === Node.ELEMENT_NODE) {
                            // Look for links that haven't been processed yet
                            if (node.matches('a[href^="/"]:not([data-url-processed])') || 
                                node.querySelector('a[href^="/"]:not([data-url-processed])')) {
                                shouldUpdate = true;
                            }
                        }
                    });
                }
            });
            
            if (shouldUpdate) {
                this.fixReactableUrls();
            }
        });

        // Observe only reactable elements, and set up observer for new reactables
        const observeReactables = () => {
            document.querySelectorAll('.reactable, [class*="reactable"]').forEach(reactable => {
                if (!reactable.hasAttribute('data-url-observer')) {
                    observer.observe(reactable, {
                        childList: true,
                        subtree: true
                    });
                    reactable.setAttribute('data-url-observer', 'true');
                }
            });
        };

        // Initial observation
        observeReactables();

        // Also watch for new reactable elements being added
        const bodyObserver = new MutationObserver(() => {
            observeReactables();
        });
        
        bodyObserver.observe(document.body, {
            childList: true,
            subtree: true
        });
    }
}

// Initialize the URL handler
// Enable debug mode with ?debug=url in the URL or on localhost
const urlParams = new URLSearchParams(window.location.search);
const isLocalhost = window.location.hostname === 'localhost' || window.location.hostname === '127.0.0.1';
const debugMode = urlParams.get('debug') === 'url' || isLocalhost;

new URLHandler(debugMode);

// Also provide a global function for manual URL adjustment
window.adjustUrl = function(path) {
    const handler = new URLHandler(false);
    return handler.adjustPath(path);
};
