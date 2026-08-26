# Barriers to Full-Arch Rehabilitation — survey pathway v2 (Shiny)

Working implementation of the v2 pathway: single instrument, single consent,
screener-driven routing, randomised BWS/DCE module order, dual-response DCE,
and write-through persistence with an Excel export.

Only hard dependency is `shiny`. Everything else — the experimental designs,
the storage layer, the xlsx writer — is base R, so the app runs unchanged on a
server, in shinylive in the browser, or headlessly under `Rscript`.

```
app/
  app.R                 entry point: state machine, persistence, admin route
  R/00-config.R         versions, burden model, runtime switches, theme
  R/01-items.R          item bank; VERBATIM_REQUIRED markers on AOHS and MDAS
  R/02-design-dce.R     paired dual-response DCE, exhaustive D-optimal search
  R/03-design-bws.R     catalogue of balanced BIBDs for the BWS module
  R/04-flow.R           routing and flow construction
  R/05-store.R          append-only storage: memory | csv | postgrest
  R/06-xlsx.R           dependency-free multi-sheet xlsx writer
  R/07-ui-pages.R       render / validate / capture, one triple per page type
  R/08-admin.R          fielding monitor, quota tracking, burden calibration
dev/
  check.R               49 assertions on designs, routing, storage, export
  simulate.R            synthetic respondents through the real pipeline
sql/schema.sql          Supabase tables, latest-revision views, insert-only RLS
```

## Run it

```r
install.packages("shiny")
shiny::runApp("app")
```

```bash
Rscript dev/check.R                    # assertions — run before locking
Rscript dev/simulate.R 300             # populate dev/sim-data with fake responses
```

URL parameters: `?admin=<key>` (fielding monitor), `?dev=1` (developer view on
the thank-you page), `?bws_items=7`, `?dce_tasks=6`, `?dce_block=1`, `?split=1`,
`?src=fb` (records recruitment source).

The admin route **fails closed**: it comes from the `SURVEY_ADMIN_KEY`
environment variable and, if that is unset, the route does not exist. There is
deliberately no default. This repository is public and the admin panel exports
the entire response set, so a committed default would be a published credential.
Set it in the server environment (Posit Connect / shinyapps.io / systemd unit),
never in the repository.

## Architecture

The screener is the only page that changes the shape of what follows. Once it
is answered, `flow_build()` returns a plain list of page descriptors — routing,
path-specific item, module order, the respondent's BWS sets and DCE tasks — and
the server is then just a pointer into that list. Consequences worth having:

- The exact page sequence a respondent saw is reconstructible from the stored
  record, which is what you need when a reviewer asks how the arms differed.
- Routing is testable without a browser. `dev/simulate.R` drives the real
  `validate_page()` and `capture_page()` functions, so a routing bug fails in CI
  rather than in the field.
- Adding a module means adding three cases to `07-ui-pages.R`, not editing the
  server.

Every page type has a matching `render_page` / `validate_page` / `capture_page`
triple. The validate and capture halves are pure functions of `(page, input)`,
which is why the simulator can exercise them.

## Storage

Append-only, four tidy tables, keyed on `rid`. Nothing is updated in place: if
a respondent goes Back and changes an answer, that writes a new row with a
higher `rev`, and readers collapse to the last revision per key. Concurrent
writes are therefore safe without transactions, and the audit trail survives.

| table | grain |
|---|---|
| `respondents` | one row per revision — routing, arm, config, status, timings |
| `items` | one row per item answered (screener, core, demographics) |
| `dce` | **one row per alternative per task** — two rows per choice set |
| `bws` | **one row per item shown per set**, with best/worst indicators |

The `dce` and `bws` shapes are already what `mlogit`, `apollo` and
`support.BWS` expect, so nothing has to be reshaped at analysis time.

Writes happen on every page advance, so partial responses are retained and
reportable — someone who abandons at DCE task 7 leaves seven usable tasks. The
simulator injects an 18% abandonment rate specifically so the pipeline is
exercised against that case.

Backends share one interface, so the database is a deployment decision rather
than a code change:

| backend | use | needs |
|---|---|---|
| `memory` | shinylive in the browser; download only | — |
| `csv` | local runs and quick tests | — |
| `sqlite` | self-hosted Shiny Server or Posit Connect | `DBI`, `RSQLite` |
| `postgres` | Neon, Azure, RDS, Render, your own server | `DBI`, `RPostgres` |
| `postgrest` | Supabase | `httr`, `jsonlite` |

`sqlite` and `postgres` are both thin wrappers over one generic `store_dbi()`,
which also takes an arbitrary connection function for MariaDB or DuckDB.

`store_backend = "auto"` resolves in order: Supabase if its variables are set,
then `DATABASE_URL`, then `SURVEY_SQLITE`, then a writable filesystem, then
memory. Supabase and Postgres deliberately win over the filesystem probe,
because several hosts give you a writable disk that does not survive a
container restart — picking `csv` there would look like it worked and lose the
data.

Supabase grants the anon key INSERT only — no SELECT, UPDATE or DELETE — so a
leaked key cannot pull responses back out; reads need the service key, which
belongs only in an environment the browser never sees.

Export: the admin panel writes a multi-sheet workbook (README sheet plus the
four tables) or a zip of CSVs. `writexl` is used when installed; otherwise the
base-R writer in `06-xlsx.R` produces the workbook, so the handover artefact
never fails for want of a package.

## Two design findings that change the v2 open items

**1. "Shorten the BWS to ~7 sets" is not available.** Every one of the 1,716
ways of dropping six blocks from the (13,4,1) design was enumerated. The item
appearance counts take only four distinct multisets, none balanced, and one of
them leaves an item shown zero times. The same holds at every block count from
5 to 12. Balance is a property of the whole cyclic design; no clever choice of
which sets to drop recovers it.

The lever that does work is the **item count**. `03-design-bws.R` carries four
genuine BIBDs, all verified in `dev/check.R`:

| items | set size | sets | appearances | λ | burden |
|---|---|---|---|---|---|
| 7 | 4 | 7 | 4 | 2 | 2:48 |
| 9 | 3 | 12 | 4 | 1 | 4:48 |
| 11 | 5 | 11 | 5 | 2 | 4:24 |
| 13 | 4 | 13 | 4 | 1 | 5:12 |

So the burden decision belongs upstream, in the qualitative phase: decide how
many barrier items survive thematic analysis, then take the matching balanced
design. Fielding an unbalanced BWS to save two minutes buys unequal precision
across exactly the contrasts the module exists to estimate.

**2. The DCE design is exact, not heuristic.** The 4×3 factorial gives 66
pairs; dropping dominated pairs leaves 18; choosing 12 of 18 is 18,564
designs, which is small enough to enumerate. No swap algorithm, no seed, no
"approximately D-optimal".

The served design maximises the **effects-coded** determinant rather than the
linear one. Rate carries four levels precisely so curvature can be tested, and
the linear-optimal design loads on the extremes (7/5/5/7 across 6/9/12/15),
leaving the interior levels thin. The effects-optimal design gives 7/6/6/5 and
a 48% larger determinant, at the cost of ~16% relative efficiency for the
linear WTP model — the right side of that trade when non-linearity is a stated
estimand. Both criteria are computed and stored with every response.

Ties on the primary criterion (there are four) break on level balance, then on
the linear criterion, so the design is fully determined and reproducible.

Also built in: two balanced blocks of six chosen maximin so a blocked fielding
does not leave one arm materially weaker; a dominance-test task appended at a
random interior position; random A/B side assignment so position is not
confounded with terms.

## Burden

At the current configuration (13 BWS items, 12 DCE tasks, both modules) the
estimate is **16:00**, not the 13:40 in the v2 notes — the earlier figure
assumed 10 BWS sets, which is not a balanced design. The admin panel computes
the full grid; the configurations that land under 13 minutes are:

| configuration | estimate |
|---|---|
| 7 BWS items + 6 DCE tasks | 10:36 |
| 7 BWS items + 12 DCE tasks | 13:36 (just over) |
| 9 or 11 BWS items + 6 DCE tasks | 12:12 – 12:36 |
| 13 BWS items + 6 DCE tasks | 13:00 |
| split-sample, either module | ~10:24 expected |

These are a priori constants from the v2 inventory. Once completions
accumulate the admin panel shows observed medians alongside them, so the
Thursday decision can be revisited against data rather than re-argued.

## Before fielding

- **Replace the VERBATIM_REQUIRED wording.** The AOHS 2023 self-rated oral
  health item and the five MDAS stems are paraphrased placeholders so the flow
  can be exercised. A paraphrased validated instrument is not the validated
  instrument: the AOHS national benchmark and the MDAS ≥19 cut-off are only
  interpretable against the source wording. AOHS is OGL v3.0, MDAS is free with
  acknowledgement — neither needs correspondence, both need transcription.
- **Replace the BWS item list.** The 13 items in `01-items.R` are placeholders
  standing in for the qualitative phase output.
- Set `SURVEY_ADMIN_KEY` in the server environment.
- Confirm the governance route (HRA decision tool, then REC) and register the
  SAP on OSF before the instrument is locked.
- Item IDs are the join keys for the analysis pipeline and the Supabase schema.
  Once fielded they must not be renamed.
