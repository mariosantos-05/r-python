# Contributing to the Project

Thank you for considering contributing to this project! We welcome contributions from everyone. Please follow the guidelines below to help make the process smoother.

## How to Contribute

1. **Fork the repository**: Create your own copy of the repository by forking it to your GitHub account.
2. **Clone your fork**: Clone the forked repository to your local machine.
   ```bash
   git clone https://github.com/your-username/project-name.git
   ```
3. **Set up remotes**: Set up the original repository as the upstream remote. This is important because it allows you to keep your fork synchronized with the main repository. When other contributors make changes to the main repository, you'll be able to pull those changes into your fork to stay up-to-date.
   ```bash
   git remote add upstream git@github.com:UnBCIC-TP2/r-python.git
   ```
   Verify your remotes are set up correctly:
   ```bash
   git remote -v
   ```
   You should see your fork as `origin` and the main repo as `upstream`.

4. **Create a branch**: Create a new branch for your feature or bugfix.
   ```bash
   git checkout -b feature/your-feature
   ```
5. **Make your changes**: Work on your changes, ensuring that they are in line with the project's coding style.
   
6. **Add the changed files**: Before committing, add the files you changed or created using:
    ```bash
    git add <file-name>
    ```
   Or to add all files:
    ```bash
    git add .
    ```
7. **Run tests**: Before submitting your pull request, ensure all tests pass.
   ```bash
   cargo test
   ```
8. **Commit your changes**: Commit your changes with a meaningful message.
   ```bash
   git commit -m "Describe your changes"
   ```
9. **Keep your fork in sync**: Before pushing, sync your fork with the upstream repository.
   ```bash
   git fetch upstream
   git checkout <your-branch>
   git rebase upstream/main
   ```
   If there are conflicts, resolve them before continuing.

10. **Push your changes**: Push your branch to your fork on GitHub.
    ```bash
    git push origin feature/your-feature
    ```
    Note: If you rebased your branch and the push is rejected, you may need to use `--force` with caution, but this should generally be avoided when possible.

## Opening a Pull Request

- **Ensure your branch is up to date with `main`**: Follow the sync steps above before opening a pull request.
- **Create a Pull Request**: After pushing your branch to your fork, open a pull request (PR) from your branch to the `main` branch of the original repository.
- **Review and Feedback**: Once your PR is submitted, the team will review your changes. You may be asked to make some improvements or fix issues before it is merged.

### Best Practices for Opening a Pull Request

- Always provide a clear and concise description of what the PR addresses.
- Make sure your code follows the project's style guidelines.
- Ensure the code is well-tested and all tests pass.
- Use meaningful commit messages that describe the changes being made.
- For significant changes to core functionality, include a detailed description explaining:
  - The rationale behind the changes
  - Any potential impacts on existing code
  - Plans for handling deprecated code
  - Performance considerations (e.g., memory usage, reference vs. clone decisions)

### Running `cargo fmt` and `cargo test`

Before submitting a pull request, please make sure to run `cargo fmt` to format the code and `cargo test` to ensure that all tests pass.

   ```bash
    cargo fmt
    cargo test
   ```

Thank you for helping us improve the project!