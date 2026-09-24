# Neonatal Error Reports

Dockerised R pipeline that pulls neonatal records from REDCap, runs data-quality
validation, and produces a CSV error report.

---

## What it does

1. Reads site configuration from `.env` (secrets) and `config.yml` (business rules).
2. Pulls REDCap records entered within the last `REPORT_INTERVAL` days.
3. Runs two validation batches:
   - `validate_data_entry` — per-field validation.
   - `validate_data_in_branching_logic` — cross-field / groupwise validation.
4. Writes `tmp/NeonatalErrorReport.csv` on the host.

The container is built from `rocker/r-ver:4.1.2`; packages are installed from
`renv.lock` into a persistent Docker volume, so rebuilds are fast.

---

## Prerequisites

On the machine that will run the pipeline:

- **Docker Engine** ≥ 20.10 — <https://docs.docker.com/engine/install/>
- **Docker Compose v2** (the `docker compose` command, not `docker-compose`) — <https://docs.docker.com/compose/install/>
- **Git**
- ~4 GB free disk for the R library volume
- Network access to the REDCap host (default `https://nbo.kemri-wellcome.org/api/`)

Check:

```bash
docker --version
docker compose version
git --version

## Clone the repository
git clone https://github.com/franklinokech/neonatal-error-reports.git
cd neonatal-error-reports

## Create .env 
cp .env.example .env

# ---- REDCap ----
REDCAP_API_URL=https://nbo.kemri-wellcome.org/api/
REDCAP_API_TOKEN=<paste the real token here>
ID_VAR=id
DATE_VAR=date_today
HOSP_VAR=hosp_id
SURROGATE_ID_VAR=ipno

# ---- Report scope ----
REPORT_INTERVAL=14
# Single ID: HOSP_TO_VALIDATE=71
# Several:   HOSP_TO_VALIDATE=71,53,58
# All:       HOSP_TO_VALIDATE=
HOSP_TO_VALIDATE=71

# ---- Fetch behaviour ----
LOCAL=true
CHUNKED=true
CHUNK_SIZE=100

# ---- Runtime ----
R_WORKERS=2

# ---- Build arg (needed only if RedcapData is private) ----
GITHUB_PAT=<paste a GitHub personal access token>

# make the shell script exec
chmod +x run-report.sh

# RUN
sh run-report.sh

# UPDATE the .desktop file paths

mkdir -p ~/.local/share/applications
cp neonatal-report.desktop ~/.local/share/applications/
chmod +x ~/.local/share/applications/neonatal-report.desktop

# Refresh the desktop database
update-desktop-database ~/.local/share/applications 2>/dev/null || true
