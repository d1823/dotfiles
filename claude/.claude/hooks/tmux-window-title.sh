#!/bin/sh
# Claude Code has no hook for "session title set"; the title only exists as
# custom-title (/rename) and ai-title records appended to the transcript.

[ -n "$TMUX" ] && [ -n "$TMUX_PANE" ] || exit 0

transcript=$(jq -r '.transcript_path // empty')
[ -f "$transcript" ] || exit 0

latest() {
    grep -F "\"type\":\"$1\"" "$transcript" | tail -n 1 | jq -r ".$2 // empty"
}

title=$(latest custom-title customTitle)
[ -n "$title" ] || title=$(latest ai-title aiTitle)
[ -n "$title" ] || exit 0

name="$(basename "${CLAUDE_PROJECT_DIR:-$PWD}"): $title"

[ "$(tmux display-message -p -t "$TMUX_PANE" '#W')" = "$name" ] && exit 0
tmux rename-window -t "$TMUX_PANE" "$name"
