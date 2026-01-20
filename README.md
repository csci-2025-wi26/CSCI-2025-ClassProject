# 📊 Data Analytics Project — Team Repository

## 📋 Overview
This repository contains the collaborative work for our **2-week data analytics project**. The project is organized under a **hierarchical team structure** to ensure rapid development, clean integration, and minimal merge conflicts.

---

## 🧠 Team Structure

### Tech Lead ([Keion Vergara](https://github.com/notKeion)) & Product Owner ([Reagan Tullis](https://github.com/rrtullis))
* **Pushes** all feature branches into `main`.
* Oversees repository structure and architecture.
* Resolves cross-feature merge conflicts.
* Coordinates with one another and Sentinels to enforce timeline/quality.


### Sentinels (Feature Leads)

Each Sentinel owns one feature domain and manages a team of 5 contributors:

* **Feature A:** @aideneve15
* **Feature B:** @maddiemann
* **Feature C:** @Corban-Freese
* **Feature D:** @AlexEmert

**Responsibilities:** Review your team's Pull Requests (Remember a PR is a request for them to merge their changes into your feature branch), manage the `feature/` branch, and report daily blockers to the Core Team.

### Contributors
* Work **only** within assigned feature branches.
* Submit PRs **only** to their Sentinel’s branch.
* Keep PRs small, focused, and atomic.
* Assign a commit message to as many file(s) changed as possible.

---

## 🌳 Git Branching Strategy Examples

| Branch | Owner | Purpose |
| :--- | :--- | :--- |
| `main` | Tech Lead | Stable, production-ready code only. |
| `feature/<name>` | Sentinel | Feature-specific integration branch. |
| `feature/<name>/data` | Data Steward | Feature-specific data management branch. |
| `feature/<name>/chart` | Chart Steward | Feature-specific chart management branch. |

### 🚫 Strict Rules
* **No direct commits to `main`.**
* **Contributors may NOT open PRs to `main`, you must open them to your Sentinel’s branch.**
* **Sentinels** manage the merge from `feature/<name>/*` -> `feature/<name>`.
* **Tech Lead** manages the merge from `feature` -> `main`.

---

## 🔁 Pull Request Workflow
1.  **Contributor → Sentinel:** Create a branch from the Sentinel’s feature branch. Open a PR to that feature branch.
2.  **Sentinel → Tech Lead:** Once the feature is complete and tested, the Sentinel opens a PR to `main`.
3.  **Conflict Resolution:** Contributors attempt resolution first; Sentinels intervene; Tech Lead resolves cross-feature clashes.

---

## 🧪 Testing & Repository Structure
Before any PR, code must be reproducible and match documented schemas.

```text
├── data/           # raw, processed, external
├── scripts/        # R scripts, analysis, modeling, viz.
├── scratch/        # Everyone's scratch work, notebooks, etc.
├── docs/           # Data dictionary, assumptions, architecture
├── tests/          # Validation scripts (if needed)
└── README.md
```

### 2. The Pull Request Template
```md

## 📝 Summary
Briefly describe what this PR does and why it is needed.

---

## 🛠 Type of Change
- [ ] 🐛 Bug fix
- [ ] ✨ New feature
- [ ] 🧹 Refactor
- [ ] 📚 Documentation update

## 🧩 Feature / Component
Which feature or module does this affect? (e.g., Feature A - ETL)

---

## 📑 Changes Made
- [Item 1]
- [Item 2]

---

## 🧪 How to Test
Describe how a reviewer can test or validate these changes (e.g., "Run `src/shiny/some_file.R`").

---

## ⚠️ Data / Schema Impact
- [ ] No data schema changes
- [ ] Data schema changes (explain below)

*Notes on impact:*

---

## 🧠 Assumptions & Notes
List any assumptions, limitations, or follow-ups.

---

## ✅ Checklist
- [ ] Code runs without errors.
- [ ] No unnecessary files included (.DS_Store, temp files, etc.).
- [ ] Naming and structure follow repo standards.
- [ ] I have tested my changes.
- [ ] I have notified my **Sentinel** of this PR.

---

## 📸 Screenshots / Outputs (if applicable)
*Attach plots, tables, or outputs that help validate this PR.*
```