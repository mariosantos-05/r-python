# Contribuindo com o Projeto

Obrigado por considerar contribuir com este projeto! Aceitamos contribuições de todos. Por favor, siga as diretrizes abaixo para tornar o processo mais suave.

## Como Contribuir

1. **Faça um fork do repositório**: Crie sua própria cópia do repositório fazendo um fork para sua conta do GitHub.
2. **Clone seu fork**: Clone o repositório bifurcado para sua máquina local.
   ```bash
   git clone https://github.com/seu-usuario/nome-do-projeto.git
   ```
3. **Configure os remotos**: Configure o repositório original como o remoto upstream. Isso é importante porque permite que você mantenha seu fork sincronizado com o repositório principal. Quando outros contribuidores fizerem alterações no repositório principal, você poderá puxar essas alterações para seu fork para manter-se atualizado.
   ```bash
   git remote add upstream git@github.com:UnBCIC-TP2/r-python.git
   ```
   Verifique se seus remotos estão configurados corretamente:
   ```bash
   git remote -v
   ```
   Você deve ver seu fork como `origin` e o repositório principal como `upstream`.

4. **Crie uma branch**: Crie uma nova branch para sua funcionalidade ou correção de bug.
   ```bash
   git checkout -b feature/sua-funcionalidade
   ```
5. **Faça suas alterações**: Trabalhe em suas alterações, garantindo que estejam de acordo com o estilo de codificação do projeto.
   
6. **Adicione os arquivos alterados**: Antes de fazer commit, adicione os arquivos que você alterou ou criou usando:
    ```bash
    git add <nome-do-arquivo>
    ```
   Ou para adicionar todos os arquivos:
    ```bash
    git add .
    ```
7. **Execute os testes**: Antes de enviar seu pull request, certifique-se de que todos os testes passam.
   ```bash
   cargo test
   ```
8. **Faça commit das alterações**: Faça commit de suas alterações com uma mensagem significativa.
   ```bash
   git commit -m "Descreva suas alterações"
   ```
9. **Mantenha seu fork sincronizado**: Antes de fazer push, sincronize seu fork com o repositório upstream.
   ```bash
   git fetch upstream
   git checkout <sua-branch>
   git rebase upstream/main
   ```
   Se houver conflitos, resolva-os antes de continuar.

10. **Faça push das alterações**: Faça push da sua branch para seu fork no GitHub.
    ```bash
    git push origin feature/sua-funcionalidade
    ```
    Nota: Se você fez rebase da sua branch e o push for rejeitado, você pode precisar usar `--force` com cautela, mas isso deve geralmente ser evitado quando possível.

## Abrindo um Pull Request

- **Certifique-se de que sua branch está atualizada com `main`**: Siga os passos de sincronização acima antes de abrir um pull request.
- **Crie um Pull Request**: Após fazer push da sua branch para seu fork, abra um pull request (PR) da sua branch para a branch `main` do repositório original.
- **Revisão e Feedback**: Uma vez que seu PR é enviado, a equipe revisará suas alterações. Você pode ser solicitado a fazer algumas melhorias ou corrigir problemas antes que seja mesclado.

### Melhores Práticas para Abrir um Pull Request

- Sempre forneça uma descrição clara e concisa do que o PR aborda.
- Certifique-se de que seu código segue as diretrizes de estilo do projeto.
- Garanta que o código está bem testado e todos os testes passam.
- Use mensagens de commit significativas que descrevem as alterações sendo feitas.
- Para mudanças significativas na funcionalidade principal, inclua uma descrição detalhada explicando:
  - A lógica por trás das mudanças
  - Quaisquer impactos potenciais no código existente
  - Planos para lidar com código depreciado
  - Considerações de desempenho (por exemplo, uso de memória, decisões de referência vs. clone)

### Executando `cargo fmt` e `cargo test`

Antes de enviar um pull request, certifique-se de executar `cargo fmt` para formatar o código e `cargo test` para garantir que todos os testes passem.

   ```bash
    cargo fmt
    cargo test
   ```

Obrigado por nos ajudar a melhorar o projeto!