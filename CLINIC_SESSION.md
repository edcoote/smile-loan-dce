# Clinic session — start-up and close-down

Repo: `C:\Users\EdwardCoote\OneDrive\smile_loan_dce`
Store: `C:\Users\EdwardCoote\21D-research\barriers`
Export: `C:\Users\EdwardCoote\OneDrive\smile_loan_dce\results\responses_live.xlsx`

Run commands **one at a time**. Pasting multi-line blocks into PowerShell
produces `>>` continuation prompts and mangles quoting.

---

## Before the first patient

**1. Power settings.** Set the laptop to never sleep while plugged in.
Windows suspending mid-session kills the app and ends the clinic.

**2. Open PowerShell** (or the VS Code terminal) and check the store location:

```powershell
$env:SURVEY_DATA_DIR
```

Must echo `C:\Users\EdwardCoote\21D-research\barriers`.

If it prints nothing, set it for this window and remember it dies with the
terminal:

```powershell
$env:SURVEY_DATA_DIR = "C:\Users\EdwardCoote\21D-research\barriers"
```

**3. Go to the repo:**

```powershell
cd C:\Users\EdwardCoote\OneDrive\smile_loan_dce
```

**4. Start the app:**

```powershell
Rscript -e "shiny::runApp('app', launch.browser=TRUE, port=4407)"
```

**5. CHECK THE STORE LINE.** Above `Listening on` you must see:

```
[store] backend=csv  location=C:\Users\EdwardCoote\21D-research\barriers
```

STOP if it says `backend=memory` — nothing will be saved.
STOP if the location ends `smile_loan_dce\data` — that is the repo, inside
OneDrive. Press `Ctrl+C`, fix step 2, start again.

**6. Open the kiosk URL** in the browser:

```
http://127.0.0.1:4407/?kiosk=1
```

**7. Dry run.** Complete one pass yourself. Then, in a SECOND terminal:

```powershell
Get-Content C:\Users\EdwardCoote\21D-research\barriers\respondents.csv | Measure-Object -Line
```

More than one line means it is writing. Then clear the test:

```powershell
Remove-Item C:\Users\EdwardCoote\21D-research\barriers -Recurse
```

It recreates empty on the next write. Do this before real patients — a test row
is indistinguishable from a genuine respondent afterwards, and there is no
defensible way to exclude it at analysis.

---

## During the session

**Leave the first terminal alone.** It is the server. Closing it, pressing
`Ctrl+C`, or letting the machine sleep ends the session.

**Between patients:** press **Start a new response** on the thank-you page, or
wait 90 seconds for the automatic reset. Both reload the session and issue a
fresh respondent id, so nothing carries over.

**If someone stops halfway:** do nothing. Their answers are already saved and
flagged `partial`. Reset and carry on.

**If the browser closes:** reopen `http://127.0.0.1:4407/?kiosk=1`. The app is
still running.

**If the terminal is closed by accident:** restart from step 3. Responses
already written are safe on disk; only the person mid-survey is lost.

---

## At the end of the session

**1. Stop the app:** `Ctrl+C` in the first terminal.

**2. Back up:**

```powershell
Rscript dev/backup.R
```

**3. Export the workbook:**

```powershell
Rscript dev/export-to-shared.R "C:\Users\EdwardCoote\OneDrive\smile_loan_dce\results\responses_live.xlsx"
```

Both print a `[store]` line. If either names `smile_loan_dce\data`, it read the
wrong store — set `SURVEY_DATA_DIR` in that terminal and run it again. The
export will say "No responses yet" rather than failing, so check the line.

**4. Confirm nothing leaked into git:**

```powershell
git status
```

Nothing new should appear. If `data\`, `results\` or any `.csv` shows as
untracked, do not commit. The repository is public and participant data
reaching it would be a notifiable breach.

**5. Delete the repo data folder if it reappeared:**

```powershell
Remove-Item C:\Users\EdwardCoote\OneDrive\smile_loan_dce\data -Recurse -Force
```

---

## Troubleshooting

| Symptom | Cause and fix |
|---|---|
| `[store] backend=memory` | Store path unwritable. Check `$env:SURVEY_DATA_DIR`, restart. |
| `[store] location=...\smile_loan_dce\data` | Variable not set in this terminal. `Ctrl+C`, set it, restart. |
| No `[store]` line at all | Running old code. Pull the updated files. |
| Export says "No responses yet" | Variable not set in the export terminal. |
| `R : ... 'e' is ambiguous` | `R` is a PowerShell alias. Use `Rscript`. |
| Prompt returns straight after `Listening on` | App exited. Nothing will record. |
| Port already in use | An earlier app is still running. `Get-Process Rscript \| Stop-Process` |
| Edits to wording not showing | No hot reload. `Ctrl+C`, start again. |
| `>>` prompt appears | Multi-line paste. Press `Ctrl+C`, run one line at a time. |

---

## Two-minute pre-clinic check

1. `$env:SURVEY_DATA_DIR` echoes the right path
2. `[store]` line names `21D-research\barriers`
3. One test pass writes a row
4. Test data cleared

Every failure so far would have been caught by steps 1 and 2.

---

## Not yet cleared for real respondents

Run `Rscript dev/lock-check.R` before fielding. Outstanding at last check:

- BWS items are placeholders pending the qualitative phase
- Six of thirteen items sit below the seven-item cut and are never shown
- REC approval and the OSF-registered SAP
