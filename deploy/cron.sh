#!/bin/bash
# Single deploy entry point: bake the model views, build the app image, push it
# to Docker Hub and roll the Hugging Face Space onto the new digest.
#
# Run this directly (cron runs `deploy/cron.sh`). Env:
#   DRY_RUN=1      build nothing, touch nothing
#   BUILD_ONLY=1   build the image locally, skip Docker Hub push and HF deploy
#   DEPLOY_CADENCE cadence recorded on the util.update_log row (default: all)
#
# The bake reads the public view layer through NBA_DB_SECTION (default
# jetson-read). Nothing is queried at app runtime.

set -uo pipefail

export PATH=/usr/local/bin:/usr/bin:/bin

REPO_DIR="${REPO_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")/.." && pwd)}"
cd "$REPO_DIR" || exit 1

source deploy/env.sh

_remote="$(git -C "$REPO_DIR" config --get remote.origin.url 2>/dev/null)"
_repo="${_remote##*/}"
_repo="${_repo%.git}"
UPDATE_LOG_SOURCE="${UPDATE_LOG_SOURCE:-${_repo:-scs.nba.mod.model_dash}}"
export UPDATE_LOG_SOURCE
export DEPLOY_CADENCE="${DEPLOY_CADENCE:-${1:-all}}"

DOCKERHUB_USER="${DOCKERHUB_USER:-shaggycamel}"
IMAGE_NAME="${IMAGE_NAME:-scs.nba.mod.model_dash}"
TAG="${TAG:-latest}"
FULL_IMAGE="${DOCKERHUB_USER}/${IMAGE_NAME}:${TAG}"
R_IMAGE="${R_IMAGE:-scs.nba.fty.league_dash_base:latest}"
CREDS_FILE="${CREDS_FILE:-${SCS_HUB_CREDENTIALS:-$HOME/.config/scs_hub_credentials.ini}}"
NBA_DB_SECTION="${NBA_DB_SECTION:-jetson-read}"
NBA_UPDATE_LOG_SECTION="${NBA_UPDATE_LOG_SECTION:-jetson-write}"
export NBA_DB_SECTION NBA_UPDATE_LOG_SECTION

KICKOFF="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
export LOG_KICKOFF="$KICKOFF"

LOCK_FILE="${LOCK_FILE:-${TMPDIR:-/tmp}/scs.nba.mod.model_dash.lock}"
exec 9>"$LOCK_FILE"
if ! flock -w 3600 9; then
  printf 'Another run is still in progress after 1h; skipping this tick.\n' >&2
  exit 1
fi

if [ "${DRY_RUN:-0}" != "1" ]; then
  : "${DOCKERHUB_TOKEN:?DOCKERHUB_TOKEN not set}"
  : "${HUGGINGFACE_TOKEN:?HUGGINGFACE_TOKEN not set}"
fi

step() { printf '\n▶ %s\n\n' "$*"; }

run_r() {
  if [ ! -f "$CREDS_FILE" ]; then
    printf '✘ credentials file not found: %s\n' "$CREDS_FILE" >&2
    return 1
  fi
  docker run --rm \
    -e HOME=/root \
    -e RENV_CONFIG_AUTOLOADER_ENABLED=FALSE \
    -e NBA_DB_SECTION="$NBA_DB_SECTION" \
    -e NBA_UPDATE_LOG_SECTION \
    -e LOG_KICKOFF -e LOG_SOURCE="$UPDATE_LOG_SOURCE" -e LOG_CADENCE="$DEPLOY_CADENCE" \
    -e LOG_TASK -e LOG_SUCCESS -e LOG_ERROR \
    -v "$REPO_DIR:/work" \
    -v "$CREDS_FILE:/root/.config/scs_hub_credentials.ini:ro" \
    -w /work \
    "$R_IMAGE" "$@"
}

log_row() {
  LOG_TASK="dashboard:model_dash" LOG_SUCCESS="$1" LOG_ERROR="${2:-}" \
    run_r Rscript deploy/log_update.R >/dev/null 2>&1 || true
}

finish() {
  local status=$?
  if [ "${DRY_RUN:-0}" = "1" ]; then
    exit "$status"
  fi
  if [ "$status" -ne 0 ]; then
    log_row 0 "deploy failed"
  else
    log_row 1 ""
  fi
  if docker image inspect "$R_IMAGE" >/dev/null 2>&1; then
    run_r sh -c "chown -R $(id -u):$(id -g) /work" >/dev/null 2>&1 || true
  fi
  exit "$status"
}
trap finish EXIT

step "Checking base image ${R_IMAGE}"
if ! docker image inspect "$R_IMAGE" >/dev/null 2>&1; then
  printf '✘ %s not found — build the league dashboard base image first\n' "$R_IMAGE" >&2
  exit 1
fi

step "Baking model views"
if [ "${DRY_RUN:-0}" = "1" ]; then
  echo "[dry-run] skip bake"
else
  run_r sh -c 'rm -f data/*.rda ./*.tar.gz'
  run_r Rscript ./data-raw/_generate_all.R || exit 1
fi

step "Building package tarball"
if [ "${DRY_RUN:-0}" = "1" ]; then
  echo "[dry-run] skip build"
else
  run_r sh -c 'rm -rf /tmp/pkgbuild && mkdir -p /tmp/pkgbuild && cp -a /work/. /tmp/pkgbuild/ \
    && rm -f /tmp/pkgbuild/.profile /tmp/pkgbuild/.deploy.env \
    && cd /tmp/pkgbuild && R CMD build . && cp /tmp/pkgbuild/*.tar.gz /work/' || exit 1
fi

step "Building image ${FULL_IMAGE}"
if [ "${DRY_RUN:-0}" = "1" ]; then
  echo "[dry-run] skip docker build"
else
  docker build -f ./docker/Dockerfile -t "$FULL_IMAGE" . || exit 1
fi

if [ "${BUILD_ONLY:-0}" = "1" ]; then
  step "BUILD_ONLY: stopping before push and deploy"
  exit 0
fi

if [ "${DRY_RUN:-0}" = "1" ]; then
  source deploy/hf.sh
  adapter_provision "$FULL_IMAGE"
  adapter_deploy "$FULL_IMAGE"
  step "DRY_RUN complete"
  exit 0
fi

step "Logging in to Docker Hub"
echo "$DOCKERHUB_TOKEN" | docker login -u "$DOCKERHUB_USER" --password-stdin || exit 1

step "Pushing ${FULL_IMAGE}"
docker push "$FULL_IMAGE" || exit 1

step "Deploying to Hugging Face"
# shellcheck source=/dev/null
source deploy/hf.sh
adapter_provision "$FULL_IMAGE" || exit 1
adapter_deploy "$FULL_IMAGE" || exit 1

step "Deployed $(adapter_url)"
