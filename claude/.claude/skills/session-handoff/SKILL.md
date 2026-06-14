---
name: session-handoff
description: Use when starting a work session and resuming context on a GitHub issue (e.g. "catch me up on #N", "let's continue on issue N"), or when ending a session before stopping (e.g. "wrap up", "let's stop here"). Applies when there is no other mechanism to preserve reasoning across irregular work blocks.
---

# Session handoff

A session-scoped loop that fights context loss across irregular work blocks. Two flows: **Resume** (start of session) and **Wrap** (end of session).

Runtime parameters: `<REPO>` (e.g. `d1823/dotfiles`), `<N>` (issue number), `<OWNER>` (e.g. `d1823`), `<PROJECT_NUMBER>` (e.g. `1`), `<REPO_ROOT>` (local checkout path).

## Resume

Trigger: the user starts a session and names an issue (e.g. "catch me up on #N").

1. **Read the issue** — title, body, and current board status. The plain view includes a `projects: <Project> (<Status>)` line, so status comes for free here:

   ```bash
   gh issue view <N> -R <REPO>
   ```

   Then read the comment thread, where prior `## Session handoff` comments live. Note: `--comments` shows *only* comments (not the body) and prints nothing when there are none, so it is a separate call:

   ```bash
   gh issue view <N> -R <REPO> --comments
   ```

2. **Board status** already appears inline in step 1 as `projects: <Project> (<Status>)`. If you need it programmatically or it is absent, query the board directly:

   ```bash
   gh project item-list <PROJECT_NUMBER> --owner <OWNER> --limit 800 --format json \
     | python3 -c "import sys,json; d=json.load(sys.stdin); print(next((i.get('status','(no status)') for i in d['items'] if i.get('content',{}).get('number')==<N>),'(not on board)'))"
   ```

   To find `<OWNER>`/`<PROJECT_NUMBER>`: owner is the repo's org or user (`gh repo view <REPO> --json owner -q .owner.login`); list projects with `gh project list --owner <OWNER>`. If there is exactly one project, use it.

3. **Reconstruct context** from the most recent comment titled `## Session handoff` — read its **Done / Why / Stopped at / Next step** fields. If no handoff comment exists, fall back to the issue body + commit history.

4. **Confirm back to the user before touching anything:** state where work stopped and the proposed next step, and wait for the go-ahead.

## Wrap

Trigger: the user is stopping for the session (e.g. "wrap up"). Work is anchored to issue `<N>` (the one from Resume, or ask which issue this session served).

1. **Diff the session's changes:**

   ```bash
   git -C <REPO_ROOT> status --short
   git -C <REPO_ROOT> diff            # unstaged
   git -C <REPO_ROOT> diff --staged   # staged
   ```

   Separate the hunks **you** wrote this session (you have a record of your own Edit/Write calls) from everything else. **The rest is the user's manual work — the interview target.** Also include any of your own hunks whose rationale was never stated aloud.

2. **Interview — 2 to 3 targeted questions about ONLY the delta.** Be specific to the hunk, never "summarize the session." Good: "You changed `X` from what I wrote to `Y` — what was the reason?" Stop at 3 questions; this must stay cheap.

3. **Route each answer by kind:**
   - **Decision-why** (why this approach / status flow / field exists) → goes in the issue comment (step 4).
   - **Code-local-why** (why this specific line/guard exists) → write it as a **code comment** next to the line, or into the **commit body**. Do not put code-local reasoning in the issue.

4. **Post the session-handoff comment** on the issue:

   ```bash
   gh issue comment <N> -R <REPO> --body "$(cat <<'EOF'
   ## Session handoff — <YYYY-MM-DD>

   **Done:** <what changed this session, 1-3 bullets>

   **Why:** <decision-why extracted in the interview>

   **Stopped at:** <where work paused>

   **Next step:** <the single next concrete action>
   EOF
   )"
   ```

5. **Set the board status:**

   ```bash
   PROJECT_ID=$(gh project list --owner <OWNER> --format json | python3 -c "import sys,json; d=json.load(sys.stdin); print(next(p['id'] for p in d['projects'] if p['number']==<PROJECT_NUMBER>))")
   FIELD_ID=$(gh project field-list <PROJECT_NUMBER> --owner <OWNER> --format json | python3 -c "import sys,json; d=json.load(sys.stdin); print(next(f['id'] for f in d['fields'] if f['name']=='Status'))")
   OPTION_ID=$(gh project field-list <PROJECT_NUMBER> --owner <OWNER> --format json | python3 -c "import sys,json; d=json.load(sys.stdin); f=next(x for x in d['fields'] if x['name']=='Status'); print(next(o['id'] for o in f['options'] if o['name']=='<STATUS_NAME>'))")
   ITEM_ID=$(gh project item-list <PROJECT_NUMBER> --owner <OWNER> --limit 800 --format json | python3 -c "import sys,json; d=json.load(sys.stdin); print(next((i['id'] for i in d['items'] if i.get('content',{}).get('number')==<N>),''))")
   [ -z "$ITEM_ID" ] && echo "Issue <N> is not on the board — add it there before setting status" || gh project item-edit --id "$ITEM_ID" --field-id "$FIELD_ID" --project-id "$PROJECT_ID" --single-select-option-id "$OPTION_ID"
   ```

   Use `<STATUS_NAME>` = `In Progress` if work is mid-flight, `Done` if the issue is complete.

6. **Confirm to the user:** report the comment URL and the new status.
