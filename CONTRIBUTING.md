# Contributing to GWSDAT

Thank you for your interest in contributing to GWSDAT.

We welcome bug reports, feature suggestions, documentation improvements, code contributions, and reproducible examples. Contributions that improve the scientific robustness, usability, accessibility, or maintainability of the project are particularly valuable.

## Ways to Contribute

You can contribute by:

- Reporting bugs
- Suggesting enhancements
- Improving documentation and examples
- Fixing existing issues
- Adding or improving tests
- Reviewing pull requests
- Contributing scientific validation cases

## Before You Start

For a substantial change, please open an issue before beginning development. This allows the proposed change, its scope, and its compatibility with the project to be discussed before significant effort is invested.

For small corrections, such as typographical fixes or minor documentation improvements, you may submit a pull request directly.

## Reporting an Issue

Before opening an issue, please check whether a similar issue has already been reported.

A useful bug report should include:

- A clear description of the problem
- Steps required to reproduce it
- The expected behaviour
- The actual behaviour
- A minimal reproducible example, where possible
- Relevant example data that can be shared publicly
- Your R version and package versions
- Your operating system and browser, where relevant
- Any error messages, warnings, or screenshots

Please do not include confidential, personal, commercially sensitive, or otherwise restricted data.

## Suggesting an Enhancement

Feature requests should explain:

- The problem or user need
- The proposed change
- The expected benefit
- Any alternative approaches considered
- Whether the change affects analysis, visualisation, data handling, or the user interface

Where possible, include a small example illustrating the proposed behaviour.

## Development Setup

### 1. Fork and clone the repository

Fork the repository, then clone your fork:

```bash
git clone https://github.com/WayneGitShell/GWSDAT.git
cd GWSDAT
```

Replace the repository address above with the correct address for your fork.

### 2. Create a branch

Create a focused branch for your change:

```bash
git checkout -b feature/short-description
```

Examples include:

```text
feature/add-trend-plot
fix/date-parsing-error
docs/improve-installation-guide
```

### 3. Load the Development Version

Install the required development tools if needed:

```r
install.packages("devtools")
```

Load the package directly from the source repository:

```r
devtools::load_all()
```

This command loads the current development version of the package into your R session without requiring installation. It should be run from the root directory of the repository.

If package dependencies are missing, install them with:

```r
devtools::install_deps(dependencies = TRUE)
```

Whenever you make code changes, rerun:

```r
devtools::load_all()
```

to reload the latest version of the package.

### 4. Run the project

Follow the repository's main README and development documentation to run the application or package locally.

## Making Changes

Please keep each contribution focused on one issue or improvement. Avoid including unrelated formatting changes or refactoring in the same pull request.

When changing code:

- Use clear and meaningful names
- Follow the existing project structure and coding style
- Keep functions focused and reasonably small
- Add comments where the reasoning is not obvious
- Avoid unnecessary dependencies
- Preserve backwards compatibility where practical
- Update documentation affected by the change
- Add or update tests for changed behaviour

## R Coding Style

R contributions should follow the style already used in the repository. Where no project-specific convention exists, use established R and tidyverse style conventions.

In particular:

- Use consistent indentation and spacing
- Prefer explicit, readable code over compact code
- Use descriptive object and function names
- Document exported functions
- Qualify less familiar package functions when this improves clarity
- Handle missing values and invalid inputs explicitly

You can check R style with tools such as `styler` and `lintr` if these are used by the project.

## Shiny Contributions

For changes to the Shiny application:

- Keep user-interface and server logic appropriately separated
- Use modules where they improve maintainability
- Preserve existing reactive behaviour unless the change intentionally modifies it
- Validate user inputs and provide informative messages
- Check the interface at common screen sizes
- Consider accessibility, including labels, contrast, and keyboard use
- Avoid exposing sensitive information in logs or error messages

## Scientific and Analytical Changes

Changes affecting calculations, statistical methods, spatial interpolation, temporal trends, plume visualisation, or scientific interpretation require particular care.

Such contributions should include, where applicable:

- A clear description of the method
- References supporting the method
- Assumptions and limitations
- A reproducible validation example
- Tests using known or independently calculated results
- An explanation of differences from the current implementation
- Consideration of missing values, non-detects, irregular sampling, and edge cases

Scientific results should not change silently. If a contribution changes existing outputs, describe the expected differences clearly in the pull request.

## Testing


New functionality should normally include tests covering:

- Expected behaviour
- Invalid or unusual inputs
- Missing values
- Boundary conditions
- Previously reported failures, where relevant

All tests should pass before the pull request is submitted.

## Documentation

Update the relevant documentation when changing functionality. Depending on the contribution, this may include:

- README files
- Function documentation
- Vignettes or tutorials
- Example datasets
- User-interface help text
- Installation instructions
- `NEWS.md` or release notes

Examples should be reproducible and should use data that may be shared publicly.

## Commit Messages

Use short, descriptive commit messages written in the imperative mood.

Examples:

```text
Fix date parsing for imported monitoring data
Add validation test for trend estimates
Improve documentation for spatial interpolation
```

## Submitting a Pull Request

Push your branch to your fork:

```bash
git push origin feature/short-description
```

Then open a pull request against the appropriate branch of the main repository.

The pull request should include:

- A concise summary of the change
- The issue addressed by the change
- Details of the implementation
- Evidence that tests pass
- Validation results for analytical changes
- Screenshots for visible interface changes
- Any known limitations or follow-up work

Please keep the pull request focused and respond constructively to review comments.

## Pull Request Checklist

Before submitting, confirm that:

- [ ] The contribution addresses a clearly defined issue or improvement
- [ ] The code follows the project's style and structure
- [ ] Existing tests pass
- [ ] New or changed behaviour is tested
- [ ] Scientific changes have been validated
- [ ] Documentation has been updated
- [ ] Public examples do not contain confidential or personal data
- [ ] User-interface changes have been checked locally
- [ ] The pull request describes any change to existing outputs
- [ ] Release notes have been updated, where applicable

## Data Privacy and Security

Do not commit:

- Personal data
- Confidential monitoring data
- Credentials, passwords, tokens, or API keys
- Proprietary datasets without permission
- Client, site, or facility information that is not public
- Files containing restricted metadata

Use synthetic, anonymised, or openly licensed data in tests and examples.

If you believe you have found a security vulnerability, do not disclose it in a public issue. Follow the repository's security-reporting process, normally described in `SECURITY.md`.


## Questions

For questions about contributing, open a GitHub Discussion or an issue using the most appropriate available template.

Thank you for helping improve GWSDAT.
