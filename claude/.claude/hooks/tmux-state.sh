#!/usr/bin/env bash
#
# Claude Code hook: tmux-state.sh working|done|waiting
#
# Records the session's state on its tmux window as @cc_state, which the tmux
# theme turns into tab colours and the per-story summary row. A state that
# needs attention is announced with a transient macOS banner, but only while
# the window is out of sight: a window you are looking at counts as seen.
set -euo pipefail

[ -n "${TMUX_PANE:-}" ] || exit 0

state=${1:-}
case $state in
working | done | waiting) ;;
*)
	echo "usage: tmux-state.sh working|done|waiting" >&2
	exit 1
	;;
esac

event=$(cat)
window=$(tmux display-message -p -t "$TMUX_PANE" '#{window_id}')

if [ "$state" = working ]; then
	tmux set-option -w -t "$window" @cc_state working
	exit 0
fi

if [ "$state" = waiting ]; then
	case $(jq -r '.notification_type // empty' <<<"$event") in
	idle_prompt | auth_success) exit 0 ;;
	esac
fi

clients=$(tmux list-clients -F '#{client_flags} #{window_id}')
if grep -q "focused.* $window\$" <<<"$clients"; then
	tmux set-option -wu -t "$window" @cc_state
	exit 0
fi

tmux set-option -w -t "$window" @cc_state "$state"

if [ "$state" = done ]; then
	message='Finished'
else
	message=$(jq -r '.message // "Needs your input"' <<<"$event")
fi
title=$(tmux display-message -p -t "$TMUX_PANE" '#S · #W')

osascript \
	-e 'on run argv' \
	-e 'display notification (item 2 of argv) with title (item 1 of argv)' \
	-e 'end run' \
	"$title" "$message"
