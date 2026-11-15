DEPLOY_ENV_FILE="${SCS_DEPLOY_ENV:-$HOME/.config/scs_deploy.env}"
if [ -f "$DEPLOY_ENV_FILE" ]; then
  # shellcheck disable=SC1090
  . "$DEPLOY_ENV_FILE"
fi
