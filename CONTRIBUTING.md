# Contributing to the Project

Thank you for considering contributing to this project! We welcome contributions from everyone. Please follow the guidelines below to help make the process smoother.

## How to Contribute

1. **Fork the repository**: Create your own copy of the repository by forking it to your GitHub account.
2. **Clone your fork**: Clone the forked repository to your local machine.
   ```bash
   git clone https://github.com/your-username/project-name.git
   ```
3. Create a branch: Create a new branch for your feature or bugfix.
   ```bash
    git checkout -b feature/your-feature
   ```
4. Make your changes: Work on your changes, ensuring that they are in line with the project's coding style.
   
5. Add the changed files: Before committing, add the files you changed or created using:
    ```bash
   git add <file-name>
    ```
   Or to add all files:
    ```bash
   git add .
    ```
6. Run tests: Before submitting your pull request, ensure all tests pass.
   ```bash
   cargo test
   ```
7. Commit your changes: Commit your changes with a meaningful message.
   ```bash
   git commit -m "Describe your changes"
   ```
8. Push your changes: Push your branch to your fork on GitHub.
   ```bash
   git push origin feature/your-feature
   ```

## Opening a Pull Request

- **Ensure your branch is up to date with `main`**: Before opening a pull request, make sure your branch has no conflicts with `main`. You can do this by updating your branch with the latest changes from `main`.
- **Create a Pull Request**: After pushing your branch to your fork, open a pull request (PR) from your branch to the `main` branch of the original repository.
- **Review and Feedback**: Once your PR is submitted, the team will review your changes. You may be asked to make some improvements or fix issues before it is merged.

### Best Practices for Opening a Pull Request

- Always provide a clear and concise description of what the PR addresses.
- Make sure your code follows the project's style guidelines.
- Ensure the code is well-tested and all tests pass.
- Use meaningful commit messages that describe the changes being made.

### Running `cargo fmt` and `cargo test`

Before submitting a pull request, please make sure to run `cargo fmt` to format the code and `cargo test` to ensure that all tests pass.

   ```bash
    cargo fmt
    cargo test
   ```

Thank you for helping us improve the project!