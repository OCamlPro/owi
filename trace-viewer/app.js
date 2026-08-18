let trace = [];
let selected = 0;

/*
 * Indices (into `trace`) of block_start rows whose children are
 * currently collapsed/hidden.
 */
let collapsedBlocks = new Set();

/*
 * Map of trace index -> array of ancestor block_start indices,
 * rebuilt every time we render the list (see buildTree).
 */
let blockAncestors = {};

/* ------------------------------------------------------------------
 * Helpers
 * ------------------------------------------------------------------ */

const $ = (id) => document.getElementById(id);

function el(tag, className, text) {
  const node = document.createElement(tag);
  if (className) node.className = className;
  if (text !== undefined) node.textContent = text;
  return node;
}

/*
 * The bundled highlight-keywords plugin does
 * `classes.push("keyword-" + token.content)` to add a per-keyword
 * class — but for a keyword with a nested "inside" grammar (e.g.
 * "i32.const", whose "." is itself a nested punctuation token),
 * `token.content` at that point is already-stringified HTML, not
 * plain text. The embedded quote breaks out of the class="..."
 * attribute and leaks markup as visible text. Sanitize every class
 * name Prism generates as a defensive fix, rather than hand-editing
 * the vendored prism.js.
 */
Prism.hooks.add("wrap", (env) => {
  env.classes = env.classes.map((c) => c.replace(/[^\w-]/g, ""));
});

/*
 * Sets an element's text to a WAT instruction and syntax-highlights it
 * with Prism (see prism.js's bundled "wasm" language grammar). Safe to
 * call repeatedly on the same element — `language-wasm` is idempotent
 * and Prism.highlightElement always re-tokenizes from textContent.
 */
function highlightWasm(element, text) {
  element.textContent = text;
  element.classList.add("language-wasm");
  Prism.highlightElement(element);
  return element;
}

function escapeHtml(value) {
  return String(value)
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;")
    .replaceAll("'", "&#039;");
}

/* ------------------------------------------------------------------
 * Trace list
 * ------------------------------------------------------------------ */

/*
 * Turn the flat `trace` array into a tree of steps and blocks, where a
 * block is a { start, children, end } group spanning a block_start /
 * block_end pair. Also (re)populates `blockAncestors`, a map from
 * trace index to the list of enclosing block_start indices, which is
 * used to auto-expand a step's parent blocks when it's selected.
 */
function buildTree(traceData) {
  blockAncestors = {};

  let i = 0;

  function parseLevel(ancestors) {
    const nodes = [];

    while (i < traceData.length) {
      const state = traceData[i];

      if (state.kind === "block_start") {
        const startIndex = i;

        blockAncestors[startIndex] = ancestors;
        i++;

        const children = parseLevel([...ancestors, startIndex]);

        let end = null;

        if (i < traceData.length && traceData[i].kind === "block_end") {
          blockAncestors[i] = ancestors;
          end = { state: traceData[i], index: i };
          i++;
        }

        nodes.push({
          type: "block",
          start: { state, index: startIndex },
          children,
          end,
        });
      } else if (state.kind === "block_end") {
        // Unmatched block_end: belongs to an enclosing level, so stop
        // here and let the caller consume it.
        return nodes;
      } else {
        blockAncestors[i] = ancestors;
        nodes.push({ type: "step", state, index: i });
        i++;
      }
    }

    return nodes;
  }

  return parseLevel([]);
}

function renderList() {
  const list = $("traceList");
  list.innerHTML = "";

  const tree = buildTree(trace);
  renderNodes(tree, list);

  $("stepCount").textContent = `${trace.length} steps`;
}

/*
 * Render a list of tree nodes into `container`. Blocks render their
 * start row, then a dedicated <div class="block-children"> holding
 * (recursively) everything inside the block, then their end row. That
 * wrapper div is what gives us indent guides (via CSS) and lets us
 * show/hide a block's contents by toggling its "is-collapsed" class.
 */
function renderNodes(nodes, container) {
  nodes.forEach((node) => {
    if (node.type === "step") {
      container.appendChild(renderRow(node.state, node.index));
      return;
    }

    container.appendChild(renderRow(node.start.state, node.start.index, node));

    const collapsed = collapsedBlocks.has(node.start.index);
    const childrenWrap = el(
      "div",
      `block-children ${collapsed ? "is-collapsed" : ""}`,
    );
    const childrenInner = el("div", "block-children-inner");
    childrenWrap.dataset.blockIndex = node.start.index;

    renderNodes(node.children, childrenInner);
    childrenWrap.appendChild(childrenInner);
    container.appendChild(childrenWrap);

    if (node.end) {
      container.appendChild(renderRow(node.end.state, node.end.index));
    }
  });
}

function renderRow(state, index, blockNode) {
  const button = el(
    "button",
    `trace-row kind-${state.kind} ${index === selected ? "active" : ""}`,
  );
  button.dataset.index = index;

  const content = el("span", "row-content");
  content.appendChild(getKindIcon(state.kind, blockNode));
  content.appendChild(highlightWasm(el("code", "instr"), state.instr));

  if (state.kind === "join" || state.kind === "widen") {
    content.appendChild(getKindBadge(state));
  }

  button.appendChild(content);
  button.addEventListener("click", () => selectState(index));

  return button;
}

/*
 * A short badge next to the instruction text for "join" (branches
 * merging back together) and "widen" (a loop fixpoint iteration) rows
 * — the two abstract-interpreter-specific events that would otherwise
 * be indistinguishable from a plain step at a glance.
 */
function getKindBadge(state) {
  let text = "join";
  if (state.kind === "widen") {
    text =
      state.converged === true
        ? "widen · converged"
        : state.converged === false
          ? "widen · iterating"
          : "widen";
  }

  return el("span", `badge row-badge badge-${state.kind}`, text);
}

function getKindIcon(kind, blockNode) {
  if (kind === "block_start") {
    // A <span role="button"> rather than a real <button>: the row
    // itself is already a <button>, and nesting interactive controls
    // is invalid HTML that browsers render/focus inconsistently.
    const collapsed = collapsedBlocks.has(blockNode.start.index);
    const icon = el("span", "kind-icon block-start-icon");

    icon.classList.toggle("is-expanded", !collapsed);
    icon.title = collapsed ? "Expand block" : "Collapse block";
    icon.setAttribute("role", "button");
    icon.setAttribute("tabindex", "0");
    icon.setAttribute("aria-expanded", String(!collapsed));

    /*
     * A single CSS-drawn triangle that rotates on expand/collapse,
     * rather than swapping between two different glyphs (▶/▼), which
     * render at inconsistent sizes/baselines across fonts/platforms.
     */
    icon.appendChild(el("span", "chevron"));

    const toggle = (event) => {
      // Don't also trigger the row's own click (which selects it).
      event.stopPropagation();
      event.preventDefault();
      toggleBlock(blockNode.start.index);
    };

    icon.addEventListener("click", toggle);
    icon.addEventListener("keydown", (event) => {
      if (event.key === "Enter" || event.key === " ") {
        toggle(event);
      }
    });

    return icon;
  }

  if (kind === "block_end") {
    // Same CSS-drawn triangle as the block-start icon (mirrored,
    // static), rather than the "◀" glyph, so the pair reads as a
    // matched pair instead of two differently-rendered characters.
    const icon = el("span", "kind-icon block-end-icon");
    icon.appendChild(el("span", "chevron"));
    return icon;
  }

  if (kind === "join" || kind === "widen") {
    const icon = el("span", "kind-icon");
    icon.appendChild(el("span", `kind-dot ${kind}-dot`));
    return icon;
  }

  return el("span");
}

/*
 * Show/hide the contents of a block by toggling its collapsed state
 * and re-rendering the list.
 */
function toggleBlock(startIndex) {
  if (collapsedBlocks.has(startIndex)) {
    collapsedBlocks.delete(startIndex);
  } else {
    collapsedBlocks.add(startIndex);
  }

  renderList();
}

/* ------------------------------------------------------------------
 * State details
 * ------------------------------------------------------------------ */

function selectState(index) {
  selected = index;

  const state = trace[index];
  const previousState = index > 0 ? trace[index - 1] : null;
  const previousState2 = index > 1 ? trace[index - 2] : null;

  const ancestors = blockAncestors[index] || [];
  const wasHidden = ancestors.some((a) => collapsedBlocks.has(a));

  if (wasHidden) {
    ancestors.forEach((a) => collapsedBlocks.delete(a));
    renderList();
  }

  document.querySelectorAll(".trace-row").forEach((row, i) => {
    row.classList.toggle("active", i === index);
  });

  $("emptyState").classList.add("is-hidden");
  $("details").classList.remove("is-hidden");

  $("stepLabel").textContent = getStepLabel(state);
  highlightWasm($("instruction"), state.instr);
  $("instrId").textContent = `instr_id ${state.instr_id}`;

  // `null` means the instruction has no successor state at all (it
  // traps, returns, or this path is unreachable): show a single
  // banner instead of four cards each individually saying so.
  const hasState = state.stack !== null && state.stack !== undefined;
  $("noStateBanner").classList.toggle("is-hidden", hasState);
  $("stateSections").classList.toggle("is-hidden", !hasState);

  if (hasState) {
    /*
     * A `br` jumps straight to its target: the "previous" state to diff
     * against is whatever came before the branch, two steps back, not
     * the branch instruction's own (now-discarded) state.
     */
    const diffAgainst = state.instr.startsWith("br ")
      ? null
      : previousState && previousState.instr.startsWith("br")
        ? previousState2
        : previousState;

    renderValues("stack", state.stack, diffAgainst?.stack);
    renderValues("locals", state.locals, diffAgainst?.locals);
    renderValues("callStack", state.call_stack, diffAgainst?.call_stack);
    renderValues("globals", state.globals, diffAgainst?.globals);

    $("stackCount").textContent = countLabel(state.stack);
    $("localsCount").textContent = countLabel(state.locals);
    $("callStackCount").textContent = countLabel(state.call_stack);
    $("globalsCount").textContent = countLabel(state.globals);
  }

  renderMergeInputs(state);
  renderJumpTargets(state.jts);
}

/*
 * `inputs` is populated for "join" (branches merging back together,
 * e.g. after an if/else) and "widen" (one loop fixpoint iteration)
 * steps: the named states that were combined to produce this step's
 * resulting state. Hidden entirely for every other kind. For "widen"
 * steps, `converged` additionally says whether this iteration reached
 * the fixpoint (the loop is about to exit) or will run another lap.
 */
function renderMergeInputs(state) {
  const card = $("mergeInputsCard");
  const container = $("mergeInputs");
  const convergedBadge = $("mergeInputsConverged");

  container.innerHTML = "";
  convergedBadge.textContent = "";
  convergedBadge.className = "badge";

  if (!state.inputs || state.inputs.length === 0) {
    card.classList.add("is-hidden");
    return;
  }

  card.classList.remove("is-hidden");
  $("mergeInputsTitle").textContent =
    state.kind === "widen" ? "Widen inputs" : "Join inputs";

  if (state.kind === "widen" && typeof state.converged === "boolean") {
    convergedBadge.textContent = state.converged ? "converged" : "iterating";
    convergedBadge.classList.add(
      state.converged ? "badge-converged" : "badge-iterating",
    );
  }

  const expanded = state.kind === "widen";
  state.inputs.forEach((input) => {
    container.appendChild(
      renderJumpTargetItem(
        { label: input.name, states: [input.state] },
        expanded,
      ),
    );
  });
}

/*
 * `jts` is the set of abstract states the interpreter would reach by
 * jumping to each candidate branch target from this step. It's
 * `null`/`undefined` when the instruction can't branch (e.g. a
 * `block_start`), in which case the card is hidden entirely rather
 * than shown empty. Each target is collapsed by default; expanding it
 * reveals the same Stack / Locals / Call stack / Globals breakdown
 * shown for the current state, rather than a raw dumped string.
 */
function renderJumpTargets(jts) {
  const card = $("jumpTargetsCard");
  const container = $("jumpTargets");

  container.innerHTML = "";

  if (jts === null || jts === undefined) {
    card.classList.add("is-hidden");
    return;
  }

  card.classList.remove("is-hidden");
  $("jumpTargetsCount").textContent = jts.length;

  if (jts.length === 0) {
    container.appendChild(emptyValue("empty"));
    return;
  }

  jts.forEach((jumpTarget) => {
    container.appendChild(renderJumpTargetItem(jumpTarget, true));
  });
}

function countLabel(values) {
  return values === null || values === undefined ? "—" : values.length;
}

function emptyValue(text, noState = false) {
  const node = el("div", "value empty-value", text);
  if (noState) node.classList.add("no-state");
  return node;
}

function renderJumpTargetItem(jumpTarget, expanded = false) {
  const states = jumpTarget.states || [];

  const header = el("button", `jump-target-header ${expanded ? "is-expanded" : ""}`);
  header.type = "button";
  header.setAttribute("aria-expanded", String(expanded));
  header.appendChild(el("span", "chevron"));
  header.appendChild(el("span", "jump-target-label", jumpTarget.label));
  header.appendChild(
    el(
      "span",
      "jump-target-meta muted",
      states.length === 1
        ? states[0] == null
          ? "unreachable"
          : ""
        : `${states.length} states`,
    ),
  );

  const body = el("div", `jump-target-body ${expanded ? "" : "is-collapsed"}`);
  states.forEach((state, index) => {
    body.appendChild(
      renderJumpTargetState(state, states.length > 1 ? index : null),
    );
  });

  header.addEventListener("click", () => {
    const collapsed = body.classList.toggle("is-collapsed");
    header.classList.toggle("is-expanded", !collapsed);
    header.setAttribute("aria-expanded", String(!collapsed));
  });

  const item = el("div", "jump-target-item");
  item.appendChild(header);
  item.appendChild(body);
  return item;
}

function renderJumpTargetState(state, stateIndex) {
  const wrap = el("div", "jump-target-state");

  if (stateIndex !== null) {
    wrap.appendChild(
      el("div", "jump-target-state-label muted", `State ${stateIndex + 1}`),
    );
  }

  if (state == null) {
    wrap.appendChild(emptyValue("unreachable", true));
    return wrap;
  }

  const grid = el("div", "jump-target-state-grid");

  [
    ["Stack", state.stack],
    ["Locals", state.locals],
    ["Call stack", state.call_stack],
    ["Globals", state.globals],
  ].forEach(([title, values]) => {
    const section = el("div", "jump-target-state-section");
    section.appendChild(el("div", "jump-target-state-title", title));

    const valuesContainer = el("div", "values");
    renderValuesInto(valuesContainer, values);
    section.appendChild(valuesContainer);

    grid.appendChild(section);
  });

  wrap.appendChild(grid);
  return wrap;
}

function getStepLabel(state) {
  switch (state.kind) {
    case "block_start":
      return `Block start · execution step ${state.id}`;

    case "block_end":
      return `Block end · execution step ${state.id}`;

    default:
      return `Execution step ${state.id}`;
  }
}

function renderValues(elementId, values, previousValues = null) {
  renderValuesInto($(elementId), values, previousValues);
}

function renderValuesInto(container, values, previousValues = null) {
  container.innerHTML = "";

  // `null` means there's no state at all here (e.g. the instruction
  // traps, returns, or is otherwise unreachable) — distinct from a
  // state that simply happens to be empty.
  if (values === null || values === undefined) {
    container.appendChild(emptyValue("no state", true));
    return;
  }

  if (values.length === 0) {
    const empty = emptyValue("empty");

    // If the previous state wasn't empty, the state changed.
    if (previousValues && previousValues.length > 0) {
      empty.classList.add("changed");
    }

    container.appendChild(empty);
    return;
  }

  values.forEach((value, index) => {
    const text = String(value);
    const item = el("div", "value", text);

    const type = ["i32", "i64", "f32", "f64", "v128"].find((t) =>
      text.startsWith(t),
    );
    if (type) item.classList.add(`type-${type}`);

    /*
     * Highlight this value if it didn't exist at this position before,
     * or if the value at this position changed.
     */
    const previousValue = previousValues?.[index];
    if (previousValues && previousValue !== value) {
      item.classList.add(previousValue === undefined ? "added" : "changed");
    }

    container.appendChild(item);
  });
}

/* ------------------------------------------------------------------
 * Theme toggle
 * ------------------------------------------------------------------ */

const THEME_KEY = "owi-tracer-theme";

// The inline script in <head> already set the initial theme (to avoid
// a flash of the wrong theme); this just wires up the toggle button.
$("themeToggle").addEventListener("click", () => {
  const current = document.documentElement.dataset.theme;
  const next = current === "light" ? "dark" : "light";

  document.documentElement.dataset.theme = next;
  localStorage.setItem(THEME_KEY, next);
  $("prismTheme").href = `prism/prism-${next}.css`;
});

/* ------------------------------------------------------------------
 * JSON file loading
 * ------------------------------------------------------------------ */

$("fileInput").addEventListener("change", async (event) => {
  const file = event.target.files[0];

  if (!file) {
    return;
  }

  try {
    const text = await file.text();
    const parsed = JSON.parse(text);

    if (!Array.isArray(parsed)) {
      throw new Error("The trace must be a JSON array.");
    }

    trace = parsed;
    selected = 0;

    renderList();

    if (trace.length > 0) {
      selectState(0);
    } else {
      $("emptyState").classList.remove("is-hidden");
      $("details").classList.add("is-hidden");
    }
  } catch (error) {
    $("traceList").innerHTML = `
      <div class="error">
        Could not load trace: ${escapeHtml(error.message)}
      </div>
    `;
  }
});

/* ------------------------------------------------------------------
 * Initial render
 * ------------------------------------------------------------------ */

renderList();

if (trace.length > 0) {
  selectState(0);
}

/* ------------------------------------------------------------------
 * Keyboard navigation
 * ------------------------------------------------------------------ */

document.addEventListener("keydown", (event) => {
  // Don't intercept keyboard input while interacting with form controls.
  const target = event.target;

  if (
    target instanceof HTMLInputElement ||
    target instanceof HTMLTextAreaElement ||
    target instanceof HTMLSelectElement ||
    target.isContentEditable
  ) {
    return;
  }

  if (trace.length === 0) {
    return;
  }

  let nextIndex = selected;

  switch (event.key) {
    /*
     * Previous / next execution trace entry.
     */
    case "ArrowUp":
      nextIndex = Math.max(0, selected - 1);
      break;

    case "ArrowDown":
    case "Enter":
      nextIndex = Math.min(trace.length - 1, selected + 1);
      break;

    /*
     * Previous / next block.
     *
     * A block is identified by its `block_start` entry.
     */
    case "ArrowLeft":
      nextIndex = findPreviousBlock(selected);
      break;

    case "ArrowRight":
      nextIndex = findNextBlock(selected);
      break;

    default:
      return;
  }

  event.preventDefault();

  if (nextIndex !== selected) {
    selectState(nextIndex);

    const row = document.querySelectorAll(".trace-row")[nextIndex];

    if (row) {
      row.scrollIntoView({
        behavior: "smooth",
        block: "nearest",
      });
    }
  }
});

function findPreviousBlock(index) {
  for (let i = index - 1; i >= 0; i--) {
    if (trace[i].kind === "block_start") {
      return i;
    }
  }

  return index;
}

function findNextBlock(index) {
  for (let i = index + 1; i < trace.length; i++) {
    if (trace[i].kind === "block_start") {
      return i;
    }
  }

  return index;
}
