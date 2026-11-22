# Rules for this repo

- Never push to GitHub (`origin`, `http-origin`). This is a public repo; the user pushes it themselves. Pushing `claude/*` branches to `forgejo` to open or update a PR is fine.
- Don't use awk in scripts. Use grep (-P with \K and lookarounds to extract fields), cut, sed or bash instead.
