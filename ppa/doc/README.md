# PPA Syntax

## Programs

Programs are a list of declarations, which can be

- **Axioms**

  `axiom <name> : <form>`

- **Theorems**

  ```text
  theorem <name> : <form>
  proof
    <steps>
  end
  ```

## Proofs

Proofs are a list of *steps*, which can be

- **Suppose**: Corresponds to implication introduction (=>-I)

  `suppose <hyp name> <form>`

- **Thus by**