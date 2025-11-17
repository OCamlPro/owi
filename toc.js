// Populate the sidebar
//
// This is a script, and not included directly in the page, to control the total size of the book.
// The TOC contains an entry for each page, so if each page includes a copy of the TOC,
// the total size of the page becomes O(n**2).
class MDBookSidebarScrollbox extends HTMLElement {
    constructor() {
        super();
    }
    connectedCallback() {
        this.innerHTML = '<ol class="chapter"><li class="chapter-item expanded affix "><a href="index.html">Home</a></li><li class="chapter-item expanded affix "><a href="installation.html">Installation</a></li><li class="chapter-item expanded affix "><a href="proposals.html">Supported Wasm proposals</a></li><li class="chapter-item expanded affix "><a href="comparison.html">Comparison with other tools</a></li><li class="chapter-item expanded affix "><li class="part-title">Symbolic Execution Engine</li><li class="chapter-item expanded "><a href="symex/symbolic-execution-101.html"><strong aria-hidden="true">1.</strong> Symbolic Execution 101</a></li><li class="chapter-item expanded "><a href="symex/quickstart.html"><strong aria-hidden="true">2.</strong> Quickstart</a></li><li class="chapter-item expanded affix "><li class="part-title">Bug-Finding, Testing &amp; Pen-testing</li><li class="chapter-item expanded "><a href="bugfinding/examples.html"><strong aria-hidden="true">3.</strong> Examples of Bug Finding</a></li><li class="chapter-item expanded "><a href="bugfinding/replay.html"><strong aria-hidden="true">4.</strong> Replaying a model</a></li><li class="chapter-item expanded "><a href="bugfinding/iso.html"><strong aria-hidden="true">5.</strong> Checking iso-behaviour of two modules</a></li><li class="chapter-item expanded "><a href="bugfinding/bugs-found.html"><strong aria-hidden="true">6.</strong> Bugs Found by Owi</a></li><li class="chapter-item expanded affix "><li class="part-title">Test-Case Generation</li><li class="chapter-item expanded "><a href="testcase-generation/code-coverage-criteria.html"><strong aria-hidden="true">7.</strong> Code Coverage Criteria</a></li><li class="chapter-item expanded "><a href="testcase-generation/labels.html"><strong aria-hidden="true">8.</strong> Labels</a></li><li class="chapter-item expanded affix "><li class="part-title">Verification and Proof of Programs</li><li class="chapter-item expanded "><a href="verification/eacsl.html"><strong aria-hidden="true">9.</strong> E-ACSL</a></li><li class="chapter-item expanded affix "><li class="part-title">Solver-Aided Programming</li><li class="chapter-item expanded "><a href="sap/examples.html"><strong aria-hidden="true">10.</strong> Examples of Problem Solving</a></li><li class="chapter-item expanded "><a href="sap/optimizations.html"><strong aria-hidden="true">11.</strong> How to Speed it Up</a></li><li class="chapter-item expanded "><a href="sap/comparison.html"><strong aria-hidden="true">12.</strong> Comparison to Rosette, Prolog, etc.</a></li><li class="chapter-item expanded affix "><li class="part-title">Going Further</li><li class="chapter-item expanded "><a href="further/talks-papers.html"><strong aria-hidden="true">13.</strong> Talks and Papers</a></li><li class="chapter-item expanded "><a href="further/commands.html"><strong aria-hidden="true">14.</strong> Commands and Options</a></li><li class="chapter-item expanded "><a href="further/api.html"><strong aria-hidden="true">15.</strong> API: Symbols and Helpers</a></li><li class="chapter-item expanded affix "><li class="part-title">WebAssembly Toolkit</li><li class="chapter-item expanded "><a href="wasm-toolkit/overview.html"><strong aria-hidden="true">16.</strong> Overview</a></li><li class="chapter-item expanded "><a href="wasm-toolkit/comparison.html"><strong aria-hidden="true">17.</strong> Comparison with Other Tools</a></li><li class="chapter-item expanded affix "><li class="part-title">Man pages</li><li class="chapter-item expanded "><a href="manpages/owi.html"><strong aria-hidden="true">18.</strong> owi</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="manpages/owi-analyze.html"><strong aria-hidden="true">18.1.</strong> owi analyze</a></li><li class="chapter-item expanded "><a href="manpages/owi-c.html"><strong aria-hidden="true">18.2.</strong> owi c</a></li><li class="chapter-item expanded "><a href="manpages/owi-cpp.html"><strong aria-hidden="true">18.3.</strong> owi c++</a></li><li class="chapter-item expanded "><a href="manpages/owi-fmt.html"><strong aria-hidden="true">18.4.</strong> owi fmt</a></li><li class="chapter-item expanded "><a href="manpages/owi-instrument.html"><strong aria-hidden="true">18.5.</strong> owi instrument</a></li><li class="chapter-item expanded "><a href="manpages/owi-iso.html"><strong aria-hidden="true">18.6.</strong> owi iso</a></li><li class="chapter-item expanded "><a href="manpages/owi-replay.html"><strong aria-hidden="true">18.7.</strong> owi replay</a></li><li class="chapter-item expanded "><a href="manpages/owi-run.html"><strong aria-hidden="true">18.8.</strong> owi run</a></li><li class="chapter-item expanded "><a href="manpages/owi-rust.html"><strong aria-hidden="true">18.9.</strong> owi rust</a></li><li class="chapter-item expanded "><a href="manpages/owi-script.html"><strong aria-hidden="true">18.10.</strong> owi script</a></li><li class="chapter-item expanded "><a href="manpages/owi-sym.html"><strong aria-hidden="true">18.11.</strong> owi sym</a></li><li class="chapter-item expanded "><a href="manpages/owi-validate.html"><strong aria-hidden="true">18.12.</strong> owi validate</a></li><li class="chapter-item expanded "><a href="manpages/owi-version.html"><strong aria-hidden="true">18.13.</strong> owi version</a></li><li class="chapter-item expanded "><a href="manpages/owi-wasm2wat.html"><strong aria-hidden="true">18.14.</strong> owi wasm2wat</a></li><li class="chapter-item expanded "><a href="manpages/owi-wat2wasm.html"><strong aria-hidden="true">18.15.</strong> owi wat2wasm</a></li><li class="chapter-item expanded "><a href="manpages/owi-zig.html"><strong aria-hidden="true">18.16.</strong> owi zig</a></li></ol></li><li class="chapter-item expanded "><li class="part-title">Public OCaml API</li><li class="chapter-item expanded "><a href="ocaml-api/overview.html"><strong aria-hidden="true">19.</strong> Overview</a></li><li class="chapter-item expanded "><a href="ocaml-api/custom-functions.html"><strong aria-hidden="true">20.</strong> How to Define Custom Functions</a></li><li class="chapter-item expanded "><a href="ocaml-api/odoc.html"><strong aria-hidden="true">21.</strong> Generated API Documentation</a></li><li class="chapter-item expanded affix "><li class="part-title">Hacking</li><li class="chapter-item expanded "><a href="hacking/setup.html"><strong aria-hidden="true">22.</strong> Development Setup</a></li><li class="chapter-item expanded "><a href="hacking/guidelines.html"><strong aria-hidden="true">23.</strong> Coding Guidelines</a></li><li class="chapter-item expanded "><a href="hacking/doc.html"><strong aria-hidden="true">24.</strong> Documentation</a></li><li class="chapter-item expanded "><a href="hacking/testing.html"><strong aria-hidden="true">25.</strong> Testing</a></li><li class="chapter-item expanded "><a href="hacking/benchmarking.html"><strong aria-hidden="true">26.</strong> Benchmarking</a></li><li class="chapter-item expanded affix "><li class="part-title">About</li><li class="chapter-item expanded "><a href="about/history.html"><strong aria-hidden="true">27.</strong> History of Owi</a></li><li class="chapter-item expanded "><a href="about/authors.html"><strong aria-hidden="true">28.</strong> Authors and Contributors</a></li><li class="chapter-item expanded "><a href="about/license.html"><strong aria-hidden="true">29.</strong> License</a></li><li class="chapter-item expanded "><a href="about/fundings.html"><strong aria-hidden="true">30.</strong> Funding</a></li><li class="chapter-item expanded "><a href="about/changelog.html"><strong aria-hidden="true">31.</strong> Changelog</a></li><li class="chapter-item expanded "><a href="about/users.html"><strong aria-hidden="true">32.</strong> Projects and People Using Owi</a></li></ol>';
        // Set the current, active page, and reveal it if it's hidden
        let current_page = document.location.href.toString().split("#")[0].split("?")[0];
        if (current_page.endsWith("/")) {
            current_page += "index.html";
        }
        var links = Array.prototype.slice.call(this.querySelectorAll("a"));
        var l = links.length;
        for (var i = 0; i < l; ++i) {
            var link = links[i];
            var href = link.getAttribute("href");
            if (href && !href.startsWith("#") && !/^(?:[a-z+]+:)?\/\//.test(href)) {
                link.href = path_to_root + href;
            }
            // The "index" page is supposed to alias the first chapter in the book.
            if (link.href === current_page || (i === 0 && path_to_root === "" && current_page.endsWith("/index.html"))) {
                link.classList.add("active");
                var parent = link.parentElement;
                if (parent && parent.classList.contains("chapter-item")) {
                    parent.classList.add("expanded");
                }
                while (parent) {
                    if (parent.tagName === "LI" && parent.previousElementSibling) {
                        if (parent.previousElementSibling.classList.contains("chapter-item")) {
                            parent.previousElementSibling.classList.add("expanded");
                        }
                    }
                    parent = parent.parentElement;
                }
            }
        }
        // Track and set sidebar scroll position
        this.addEventListener('click', function(e) {
            if (e.target.tagName === 'A') {
                sessionStorage.setItem('sidebar-scroll', this.scrollTop);
            }
        }, { passive: true });
        var sidebarScrollTop = sessionStorage.getItem('sidebar-scroll');
        sessionStorage.removeItem('sidebar-scroll');
        if (sidebarScrollTop) {
            // preserve sidebar scroll position when navigating via links within sidebar
            this.scrollTop = sidebarScrollTop;
        } else {
            // scroll sidebar to current active section when navigating via "next/previous chapter" buttons
            var activeSection = document.querySelector('#sidebar .active');
            if (activeSection) {
                activeSection.scrollIntoView({ block: 'center' });
            }
        }
        // Toggle buttons
        var sidebarAnchorToggles = document.querySelectorAll('#sidebar a.toggle');
        function toggleSection(ev) {
            ev.currentTarget.parentElement.classList.toggle('expanded');
        }
        Array.from(sidebarAnchorToggles).forEach(function (el) {
            el.addEventListener('click', toggleSection);
        });
    }
}
window.customElements.define("mdbook-sidebar-scrollbox", MDBookSidebarScrollbox);
