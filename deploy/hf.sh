#!/bin/bash
# Hugging Face Spaces deploy adapter -----------------------------------------
# One public Space serving the baked app image, pinned by digest so HF cannot
# serve a stale image. Requires HUGGINGFACE_TOKEN unless DRY_RUN=1.

: "${HF_OWNER:=shaggycamel}"
: "${HF_SPACE_NAME:=model-dash}"
: "${HF_APP:=modeldash}"

hf_space_id() {
  echo "${HF_OWNER}/${HF_SPACE_NAME}"
}

adapter_url() {
  echo "https://${HF_OWNER}-${HF_SPACE_NAME}.hf.space"
}

# Resolve an image tag to its immutable digest via the Docker Hub registry API.
hf_image_ref() {
  local image="$1"

  case "$image" in
    *@sha256:*) printf '%s' "$image"; return 0 ;;
  esac

  local repo tag
  repo="${image%:*}"
  tag="${image##*:}"
  if [ "$tag" = "$image" ]; then tag="latest"; fi

  local path="$repo"
  case "$repo" in
    */*) ;;
    *) path="library/$repo" ;;
  esac

  local attempt token digest
  for attempt in 1 2 3; do
    token="$(curl -s "https://auth.docker.io/token?service=registry.docker.io&scope=repository:${path}:pull" \
      | sed -n 's/.*"token":"\([^"]*\)".*/\1/p')"
    if [ -n "$token" ]; then
      digest="$(curl -sI \
        -H "Authorization: Bearer ${token}" \
        -H "Accept: application/vnd.docker.distribution.manifest.v2+json" \
        -H "Accept: application/vnd.docker.distribution.manifest.list.v2+json" \
        -H "Accept: application/vnd.oci.image.manifest.v1+json" \
        -H "Accept: application/vnd.oci.image.index.v1+json" \
        "https://registry-1.docker.io/v2/${path}/manifests/${tag}" \
        | tr -d '\r' \
        | sed -n 's/^[Dd]ocker-[Cc]ontent-[Dd]igest:[[:space:]]*\(.*\)$/\1/p')"
      if [ -n "$digest" ]; then
        printf '%s@%s' "${repo}" "${digest}"
        return 0
      fi
    fi
    sleep $((attempt * 2))
  done

  echo "[hf] ERROR: could not resolve a digest for ${image} after 3 attempts" >&2
  return 1
}

hf_space_head() {
  local repo="$1"
  curl -s -H "Authorization: Bearer ${HUGGINGFACE_TOKEN}" \
    "https://huggingface.co/api/spaces/${repo}/refs" \
    | python3 -c 'import sys,json; d=json.load(sys.stdin); print(next((b["targetCommit"] for b in d.get("branches",[]) if b["name"]=="main"), ""))'
}

hf_runtime() {
  local repo="$1"
  curl -s -H "Authorization: Bearer ${HUGGINGFACE_TOKEN}" \
    "https://huggingface.co/api/spaces/${repo}" \
    | python3 -c 'import sys,json; d=json.load(sys.stdin); r=d.get("runtime") or {}; print(r.get("sha",""), r.get("stage",""))'
}

# Wait until the Space serves the expected commit and answers HTTP. A Space
# stuck on its previous build is healed with a factory reboot.
hf_wait_healthy() {
  local repo="$1" expected="$2" timeout="${3:-900}" grace="${HF_HEAL_GRACE:-90}"
  local waited=0 healed=0 runtime sha stage url last_sha="" last_stage="" http_timeout="${HF_HTTP_TIMEOUT:-10}"
  url="$(adapter_url)"

  while [ "$waited" -lt "$timeout" ]; do
    runtime="$(hf_runtime "$repo" 2>/dev/null || true)"
    sha="${runtime%% *}"
    stage="${runtime##* }"
    last_sha="$sha"
    last_stage="$stage"

    if [ -n "$expected" ] && [ "$sha" = "$expected" ] && [ "$stage" = "RUNNING" ]; then
      if curl -fsSL --max-time "$http_timeout" -o /dev/null "${url}/" 2>/dev/null; then
        return 0
      fi
    fi

    if [ -n "$expected" ] && [ "$stage" = "RUNNING" ] && [ "$sha" != "$expected" ] \
      && [ "$waited" -ge "$grace" ] && [ "$healed" = "0" ]; then
      echo "[hf] WARN: ${repo} still on ${sha:-?}, expected ${expected:-?}; forcing factory reboot" >&2
      curl -sf -X POST "https://huggingface.co/api/spaces/${repo}/restart?factory=true" \
        -H "Authorization: Bearer ${HUGGINGFACE_TOKEN}" >/dev/null 2>&1 || true
      healed=1
    fi

    sleep 10
    waited=$((waited + 10))
  done

  echo "[hf] ERROR: ${repo} did not converge to ${expected:-?} (last runtime ${last_sha:-?} ${last_stage:-?}) within ${timeout}s" >&2
  return 1
}

# Commit a Dockerfile pinned to the pushed image's digest, with a cache-busting
# build arg. HF only rebuilds a Docker Space when its repository changes.
hf_configure_space() {
  local repo="$1"
  local image="$2"

  local image_ref
  image_ref="$(hf_image_ref "$image")" || return 1

  local stamp
  stamp="$(date -u +%Y%m%dT%H%M%SZ)-${RANDOM}${RANDOM}"

  local df_json="FROM ${image_ref}\\nARG SPACE_REBUILD=${stamp}\\nRUN echo \\\"space rebuild ${stamp}\\\"\\nHEALTHCHECK --interval=30s --timeout=5s --start-period=60s CMD curl -fsS http://localhost:7860/ >/dev/null || exit 1\\nEXPOSE 7860\\nUSER rstudio\\nCMD [\\\"R\\\", \\\"-e\\\", \\\"options('shiny.port'=7860, 'shiny.host'='0.0.0.0'); library(${HF_APP}); ${HF_APP}::run_app()\\\"]"

  printf '%s\n' \
    '{"key":"header","value":{"summary":"provision space"}}' \
    "{\"key\":\"file\",\"value\":{\"path\":\"Dockerfile\",\"content\":\"${df_json}\",\"encoding\":\"utf-8\"}}" |
    curl -sf -X POST "https://huggingface.co/api/spaces/${repo}/commit/main" \
      -H "Authorization: Bearer ${HUGGINGFACE_TOKEN}" \
      -H "Content-Type: application/x-ndjson" \
      --data-binary @- >/dev/null || return 1

  echo "[hf] ${repo} -> ${image_ref}"
}

adapter_provision() {
  local image="$1"
  local repo
  repo="$(hf_space_id)"
  local hardware="${HF_SPACE_HARDWARE:-cpu-basic}"
  local sleep_time="${HF_SPACE_SLEEP_TIME:-300}"

  if [ "${DRY_RUN:-0}" = "1" ]; then
    echo "[dry-run] provision HF space ${repo} (FROM ${image}, app ${HF_APP})"
    return 0
  fi

  : "${HUGGINGFACE_TOKEN:?HUGGINGFACE_TOKEN is required for HF provisioning}"

  local status
  status="$(curl -s -o /dev/null -w '%{http_code}' \
    -H "Authorization: Bearer ${HUGGINGFACE_TOKEN}" \
    "https://huggingface.co/api/spaces/${repo}")"

  if [ "$status" = "200" ]; then
    echo "[hf] ${repo} exists; deploy will rebuild"
    return 0
  fi

  echo "[hf] creating ${repo} (${hardware}, sleep ${sleep_time}s)"
  curl -sf -X POST "https://huggingface.co/api/repos/create" \
    -H "Authorization: Bearer ${HUGGINGFACE_TOKEN}" \
    -H "Content-Type: application/json" \
    -d "{\"type\":\"space\",\"name\":\"${HF_SPACE_NAME}\",\"private\":false,\"sdk\":\"docker\",\"hardware\":\"${hardware}\",\"sleepTimeSeconds\":${sleep_time}}" >/dev/null || return 1

  hf_configure_space "$repo" "$image" || return 1
  hf_wait_healthy "$repo" "$(hf_space_head "$repo")" "${HF_HEALTH_TIMEOUT:-900}" || return 1
}

adapter_deploy() {
  local image="$1"
  local repo
  repo="$(hf_space_id)"

  if [ "${DRY_RUN:-0}" = "1" ]; then
    echo "[dry-run] rebuild + restart HF space ${repo} (image ${image}) -> $(adapter_url)"
    return 0
  fi

  : "${HUGGINGFACE_TOKEN:?HUGGINGFACE_TOKEN is required for HF deploys}"

  hf_configure_space "$repo" "$image" || return 1

  local expected
  expected="$(hf_space_head "$repo")"
  hf_wait_healthy "$repo" "$expected" "${HF_HEALTH_TIMEOUT:-900}" || return 1
  echo "[hf] ${repo} serving ${expected:0:12}"
}
