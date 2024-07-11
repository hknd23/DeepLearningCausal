# Contributing to DeepLearningCausal

Welcome to `DeepLearningCausal` package, and thank you for considering contributing. Please note that you can contribute in many ways, 
including fixing a bug, requesting a new
feature, code patches, editing the documentation, and patch reviews. We appreciate your help in
improving any feature included in this package.

This contributing guide contains the necessary information you need to know to help build and
improve `DeepLearningCausal`. Please read and follow these guidelines as it will make both communication
and the contribution process easy and effective. We will reciprocate by fixing bugs, evaluating changes, and helping you finalize your pull requests.

## Code of Conduct

We treat the open-source community with respect and hold ourselves as well as other
contributors to high standards of communication. By contributing to this project, you agree to
uphold our following [Code of Conduct](https://github.com/hknd23/DeepLearningCausal/blob/main/code_of_conduct.md):

- Focusing on what is best for the community.
- Respect differing viewpoints and accept constructive criticisms.
- Avoid conduct that could reasonably be considered inappropriate in a professional setting.

Our responsibilities as maintainers include the right to remove, edit, or reject comments,
commits, code, issues, and other contributions that are not aligned to this [Code of Conduct](https://github.com/hknd23/DeepLearningCausal/blob/main/code_of_conduct.md).

## Bug Reports

We use GitHub issues to track public bugs. Note that a bug can be reported by simply opening a
new issue. An issue template for reporting bugs is available. Before openning a new issue,
please check whether a similar issue has already been reported.
A useful Bug Report is one that includes:

- A summary or background.
- Steps to reproduce. Please be as specific as possible in this regard.
- Give sample code and screenshots if you can. This makes it easier to understand, track and correct the
main issues that you are raising.
- Briefly explain what you expected would happen and what actually transpired (contrary
to your expectations).
- Finally, please note the steps you may have undertaken to address the bug.

## Feature Requests

We welcome any requests to add new features. The feature request issue template is
available to facilitate the process. That said, please ensure that your request to add a new
feature, for example, fits the objectives and scope of the `DeepLearningCausal`.
Please provide as much detail as possible and ensure that the feature 
that you intend to add is compatible with other features included in this package. 

## Issues

Contributors should use Issues to report problems with the library, request a new feature, or to
discuss potential changes before a PR is created. When a contributor creates a new Issue, a
template will be loaded that will guide him or her to collect and provide the necessary
information that we (the maintainers) need to investigate. If the contributor finds an Issue that
addresses the problem they are having, then he or she should add their own reproduction
information to the existing issue rather than creating a new one.

## Pull Requests

Pull requests to our libraries in `DeepLearningCausal` are more than welcome. Pull requests (PRs) are the
best way to propose changes to the codebase. In general, we use [GitHub flow](https://guides.github.com/introduction/flow/index.html) for PRs to:

- [Fork](https://docs.github.com/en/get-started/quickstart/fork-a-repo) the repository to your
own Github account and create your branch from master.

- If you have added code that should be tested, please add tests.
- Commit changes to the branch.
- If you have changed APIs, please update the documentation.
- Follow any formatting and testing guidelines specific to this repo.
- Check for errors with `devtools::check()`.
- Push changes to your fork.
- Open a PR in our repository and follow the PR template so that we can review ANY
changes.

**Please ask first** if you'd like to embark in any significant pull request. 

The precess below details the steps that you should follow if you'd like your work considered for inclusion in `DeepLearningCausal`:

1. [Fork](http://help.github.com/fork-a-repo/) the project, clone your fork,
   and configure the remotes:

   ```bash
   # Clone your fork of the repo into the current directory
   git clone https://github.com/hknd23/DeepLearningCausal
   # Navigate to the newly cloned directory
   cd DeepLearningCausal
   # Assign the original repo to a remote called "upstream"
   git remote add upstream https://github.com/<upstream-owner>/DeepLearningCausal
   ```

2. If you cloned a while ago, get the latest changes from upstream:

   ```bash
   git checkout <dev-branch>
   git pull upstream <dev-branch>
   ```

3. Create a new topic branch (off the main project development branch) to
   contain your feature, change, or fix:

   ```bash
   git checkout -b <topic-branch-name>
   ```

4. Commit changes in logical chunks. Please adhere to these [git commit
   message guidelines](http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html) or your code is unlikely be merged into the main project. Use Git's
   [interactive rebase](https://help.github.com/articles/interactive-rebase)
   feature to tidy up your commits before making them public.

5. Locally merge (or rebase) the upstream development branch into your topic branch:

   ```bash
   git pull [--rebase] upstream <dev-branch>
   ```

6. Push your topic branch up to your fork:

   ```bash
   git push origin <topic-branch-name>
   ```

7. [Open a Pull Request](https://help.github.com/articles/using-pull-requests/)
    with a clear title and description.

**IMPORTANT**: By submitting a patch, you agree to allow the project owner to
license your work under the same license as that used by the project.
