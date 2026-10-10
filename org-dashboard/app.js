"use strict";

const stateOrder = ["NEXT", "TODO", "WAIT"];
const stateLabels = { NEXT: "Next", TODO: "Backlog", WAIT: "Waiting" };

const model = {
  tasks: [],
  generatedAt: null,
  query: "",
  sources: new Set(),
  states: new Set(stateOrder),
  view: "board",
};

const elements = {
  error: document.querySelector("#error"),
  refresh: document.querySelector("#refresh"),
  search: document.querySelector("#search"),
  sourceFilters: document.querySelector("#source-filters"),
  stateFilters: document.querySelector("#state-filters"),
  summary: document.querySelector("#summary"),
  taskView: document.querySelector("#task-view"),
  toast: document.querySelector("#toast"),
  viewButtons: [...document.querySelectorAll("[data-view]")],
};

function orderedUnique(values) {
  return [...new Set(values)];
}

function contextFor(task) {
  return Array.isArray(task.breadcrumbs) && task.breadcrumbs.length
    ? task.breadcrumbs.join(" › ")
    : "Top level";
}

function datesFor(task) {
  const values = [];
  if (task.scheduled) values.push(`Scheduled ${task.scheduled}`);
  if (task.deadline) values.push(`Due ${task.deadline}`);
  if (task.timestamp) values.push(task.timestamp);
  return values.join(" · ");
}

function filteredTasks() {
  const query = model.query.trim().toLocaleLowerCase();
  return model.tasks.filter((task) => {
    if (!model.sources.has(task.source) || !model.states.has(task.state)) return false;
    if (!query) return true;
    return [task.title, task.source, contextFor(task)]
      .join(" ")
      .toLocaleLowerCase()
      .includes(query);
  });
}

function makePill(label, pressed, onClick) {
  const button = document.createElement("button");
  button.type = "button";
  button.className = "filter-pill";
  button.textContent = label;
  button.setAttribute("aria-pressed", String(pressed));
  button.addEventListener("click", onClick);
  return button;
}

function renderFilters() {
  const sources = orderedUnique(model.tasks.map((task) => task.source));
  elements.sourceFilters.replaceChildren(
    ...sources.map((source) =>
      makePill(source, model.sources.has(source), () => {
        if (model.sources.has(source)) model.sources.delete(source);
        else model.sources.add(source);
        render();
      }),
    ),
  );

  elements.stateFilters.replaceChildren(
    ...stateOrder.map((taskState) =>
      makePill(stateLabels[taskState], model.states.has(taskState), () => {
        if (model.states.has(taskState)) model.states.delete(taskState);
        else model.states.add(taskState);
        render();
      }),
    ),
  );
}

function showToast(message) {
  elements.toast.textContent = message;
  elements.toast.hidden = false;
  window.clearTimeout(showToast.timeout);
  showToast.timeout = window.setTimeout(() => {
    elements.toast.hidden = true;
  }, 2200);
}

async function openTask(task) {
  try {
    const response = await fetch("/api/open", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ id: task.id }),
    });
    if (!response.ok) throw new Error((await response.json()).error || "Could not open task");
    showToast("Opened in Emacs");
  } catch (error) {
    showError(error.message);
  }
}

function makeStateBadge(taskState) {
  const badge = document.createElement("span");
  badge.className = "state-badge";
  badge.dataset.state = taskState;
  badge.textContent = stateLabels[taskState];
  return badge;
}

function makeCard(task) {
  const card = document.createElement("button");
  card.type = "button";
  card.className = "task-card";
  card.addEventListener("click", () => openTask(task));

  const title = document.createElement("span");
  title.className = "task-title";
  title.textContent = task.title;

  const context = document.createElement("span");
  context.className = "task-context";
  context.textContent = contextFor(task);

  card.append(makeStateBadge(task.state), title, context);

  const dates = datesFor(task);
  if (dates) {
    const date = document.createElement("span");
    date.className = "task-date";
    date.textContent = dates;
    card.append(date);
  }

  const hint = document.createElement("span");
  hint.className = "open-hint";
  hint.textContent = "Open in Emacs ↗";
  card.append(hint);
  return card;
}

function makeColumn(taskState, tasks) {
  const column = document.createElement("section");
  column.className = "column";

  const header = document.createElement("header");
  header.className = "column-header";
  header.dataset.state = taskState;
  const label = document.createElement("span");
  label.textContent = stateLabels[taskState];
  const count = document.createElement("span");
  count.className = "count";
  count.textContent = String(tasks.length);
  header.append(label, count);

  const stack = document.createElement("div");
  stack.className = "card-stack";
  stack.append(...tasks.map(makeCard));
  column.append(header, stack);
  return column;
}

function renderBoard(tasks) {
  const fragment = document.createDocumentFragment();
  const sources = orderedUnique(model.tasks.map((task) => task.source)).filter((source) =>
    tasks.some((task) => task.source === source),
  );

  for (const source of sources) {
    const laneTasks = tasks.filter((task) => task.source === source);
    const lane = document.createElement("section");
    lane.className = "lane";

    const header = document.createElement("header");
    header.className = "lane-header";
    const title = document.createElement("h2");
    title.className = "lane-title";
    const mark = document.createElement("span");
    mark.className = "lane-mark";
    mark.setAttribute("aria-hidden", "true");
    const titleText = document.createElement("span");
    titleText.textContent = source;
    const count = document.createElement("span");
    count.className = "count";
    count.textContent = String(laneTasks.length);
    title.append(mark, titleText, count);
    header.append(title);

    const grid = document.createElement("div");
    grid.className = "board-grid";
    grid.append(
      ...stateOrder.map((taskState) =>
        makeColumn(taskState, laneTasks.filter((task) => task.state === taskState)),
      ),
    );
    lane.append(header, grid);
    fragment.append(lane);
  }
  return fragment;
}

function makeRow(task) {
  const row = document.createElement("button");
  row.type = "button";
  row.className = "task-row";
  row.addEventListener("click", () => openTask(task));

  const source = document.createElement("span");
  source.className = "row-source";
  source.textContent = task.source;
  const title = document.createElement("span");
  title.className = "row-title";
  title.textContent = task.title;
  const context = document.createElement("span");
  context.className = "row-context";
  context.textContent = contextFor(task);
  const date = document.createElement("span");
  date.className = "row-date";
  date.textContent = datesFor(task) || "No date";
  row.append(source, makeStateBadge(task.state), title, context, date);
  return row;
}

function renderList(tasks) {
  const list = document.createElement("div");
  list.className = "list-view";
  list.append(...tasks.map(makeRow));
  return list;
}

function renderSummary(tasks) {
  const entries = [
    [String(tasks.length), "visible"],
    ...stateOrder.map((taskState) => [
      String(tasks.filter((task) => task.state === taskState).length),
      stateLabels[taskState].toLocaleLowerCase(),
    ]),
  ];
  if (model.generatedAt) {
    const generated = new Date(model.generatedAt);
    const display = Number.isNaN(generated.valueOf()) ? model.generatedAt : generated.toLocaleString();
    entries.push(["", `updated ${display}`]);
  }

  const nodes = [];
  entries.forEach(([value, label], index) => {
    if (index) {
      const separator = document.createElement("span");
      separator.className = "summary-dot";
      separator.textContent = "•";
      nodes.push(separator);
    }
    const item = document.createElement("span");
    if (value) {
      const strong = document.createElement("strong");
      strong.textContent = value;
      item.append(strong, ` ${label}`);
    } else {
      item.textContent = label;
    }
    nodes.push(item);
  });
  elements.summary.replaceChildren(...nodes);
}

function render() {
  renderFilters();
  const tasks = filteredTasks();
  renderSummary(tasks);
  elements.viewButtons.forEach((button) => {
    button.classList.toggle("is-active", button.dataset.view === model.view);
  });

  if (!tasks.length) {
    const empty = document.createElement("p");
    empty.className = "empty-state";
    empty.textContent = "No tasks match these filters.";
    elements.taskView.replaceChildren(empty);
    return;
  }
  elements.taskView.replaceChildren(model.view === "board" ? renderBoard(tasks) : renderList(tasks));
}

function showError(message) {
  elements.error.textContent = message;
  elements.error.hidden = false;
}

async function loadTasks() {
  elements.error.hidden = true;
  const response = await fetch("/api/tasks", { cache: "no-store" });
  if (!response.ok) throw new Error("Could not load Org task data");
  const payload = await response.json();
  if (payload.schemaVersion !== 1 || !Array.isArray(payload.tasks)) {
    throw new Error("The Org task data has an unsupported format");
  }
  model.tasks = payload.tasks;
  model.generatedAt = payload.generatedAt;
  model.sources = new Set(model.tasks.map((task) => task.source));
  render();
}

elements.search.addEventListener("input", (event) => {
  model.query = event.target.value;
  render();
});

elements.viewButtons.forEach((button) => {
  button.addEventListener("click", () => {
    model.view = button.dataset.view;
    render();
  });
});

elements.refresh.addEventListener("click", async () => {
  elements.refresh.disabled = true;
  try {
    const response = await fetch("/api/refresh", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: "{}",
    });
    if (!response.ok) throw new Error((await response.json()).error || "Could not refresh Org tasks");
    await loadTasks();
    showToast("Refreshed from Org");
  } catch (error) {
    showError(error.message);
  } finally {
    elements.refresh.disabled = false;
  }
});

loadTasks().catch((error) => showError(error.message));
