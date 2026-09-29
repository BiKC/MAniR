/* Capture REAL MAniR screens from a running Shiny session.
 * Use only the bundled SYNTHETIC example, never a researcher's uploaded data.
 *
 * Run:
 *   Rscript -e "shiny::runApp('.', host='127.0.0.1', port=3838)" &
 *   npm install --no-save --no-package-lock playwright
 *   node scripts/capture-docs-screenshots.js
 *
 * Screenshots are deliberately not retouched or composited.
 */
const fs = require("node:fs");
const path = require("node:path");
const { chromium } = require("playwright");

const url = process.env.MANIR_SCREENSHOT_URL || "http://127.0.0.1:3838";
const outputDir = path.resolve("docs/images");
const screenshotOptions = { animations: "disabled", timeout: 30000 };
const maxWait = 60000;

function fail(message) {
  throw new Error("MAniR screenshot capture: " + message);
}

async function waitForText(page, selector, text) {
  await page.waitForFunction(
    ({ selector, text }) =>
      document.querySelector(selector)?.textContent?.includes(text),
    { selector, text },
    { timeout: maxWait }
  );
}

async function save(page, name) {
  await page.screenshot({
    path: path.join(outputDir, name),
    fullPage: false,
    ...screenshotOptions
  });
  const size = fs.statSync(path.join(outputDir, name)).size;
  if (size < 8000) fail(name + " looks unexpectedly empty (" + size + " bytes)");
  console.log("Captured " + name + " (" + Math.round(size / 1024) + " KiB)");
}

async function goStart(page) {
  await page.locator('a[data-toggle="tab"]')
    .filter({ hasText: /^Start here$/ }).first().click();
  await page.locator("#start_heatmap").waitFor({ timeout: maxWait });
}

async function capture() {
  fs.mkdirSync(outputDir, { recursive: true });
  const args = ["--no-sandbox", "--disable-dev-shm-usage"];
  const browser = await chromium.launch({
    headless: true,
    executablePath: process.env.CHROME_BIN || undefined,
    args
  });
  const page = await browser.newPage({
    viewport: { width: 1680, height: 1000 },
    deviceScaleFactor: 1,
    colorScheme: "light",
    reducedMotion: "reduce"
  });
  const errors = [];
  page.on("pageerror", e => errors.push(e.message));
  page.on("console", message => {
    if (message.type() === "error") console.log("Browser:", message.text());
  });

  try {
    const response = await page.goto(url, {
      waitUntil: "domcontentloaded",
      timeout: maxWait
    });
    if (!response?.ok()) fail("Shiny app returned HTTP " + response?.status());
    await page.locator("#load_example").waitFor({ timeout: maxWait });
    await page.locator("#load_example").click();
    await waitForText(page, "#research_status", "Teaching example ready");
    await waitForText(page, "#research_status", "12");
    // Give the example-loaded notification time to disappear.
    await page.waitForTimeout(4400);
    await save(page, "start-here.png");

    // Real Plotly heatmap, with the default metadata field in the example.
    await page.locator("#start_heatmap").click();
    await page.locator("#plot1_interactive .main-svg")
      .waitFor({ timeout: maxWait });
    await waitForText(page, "#plot1_intro", "12 isolates");
    await page.waitForTimeout(600);
    await save(page, "heatmap.png");

    // Real split-triangle view, both matrices ordered by the first.
    await page.locator('a[data-toggle="tab"]')
      .filter({ hasText: /^Combined \(order 1\)$/ }).first().click();
    await page.locator("#combined1_interactive .main-svg")
      .waitFor({ timeout: maxWait });
    await page.waitForTimeout(600);
    await save(page, "combined-heatmap.png");

    await goStart(page);
    await page.locator("#start_agreement").click();
    await page.locator("#agreement table").waitFor({ timeout: maxWait });
    await page.waitForTimeout(300);
    await save(page, "method-overview.png");

    await goStart(page);
    await page.locator("#start_pairs").click();
    await waitForText(page, "#pair_inspection", "0.52");
    await page.locator("#scatter img").waitFor({ timeout: maxWait });
    await page.waitForTimeout(400);
    await save(page, "pairwise-comparison.png");
    await page.locator(".pair-focus").screenshot({
      path: path.join(outputDir, "pair-inspector.png"),
      ...screenshotOptions
    });

    await goStart(page);
    await page.locator("#start_cluster").click();
    await page.locator("#cluster_summary table").waitFor({ timeout: maxWait });
    await page.locator("#cluster_overlap table").waitFor({ timeout: maxWait });
    await page.waitForTimeout(400);
    await save(page, "cluster-comparison.png");

    await goStart(page);
    await page.locator("#start_groups").click();
    await waitForText(page, "#metadata_group_status", "Exploring");
    await page.locator("#group_distributions img")
      .waitFor({ timeout: maxWait });
    await page.waitForTimeout(400);
    await save(page, "metadata-groups.png");

    await goStart(page);
    await page.locator("#start_export").click();
    await page.locator("#research_question").fill(
      "Do MALDI-TOF similarities reflect genomic ANI relationships?"
    );
    await page.locator("#research_notes").fill(
      "Synthetic teaching example. Inspect discordant isolate pairs and " +
      "check whether the selected metadata groups explain the difference. " +
      "Follow up with real spectra and sequence quality controls."
    );
    await page.waitForTimeout(300);
    await save(page, "research-notes-export.png");

    if (errors.length) {
      // Shiny/Plotly can issue nonfatal widget warnings. Retain logs for
      // review instead of claiming the screenshots are error-free.
      console.warn("Browser JS messages:", errors.join(" | "));
    }
  } finally {
    await browser.close();
  }
}

capture().catch(error => {
  console.error(error);
  process.exitCode = 1;
});
