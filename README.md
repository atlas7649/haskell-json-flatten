# Haskell JSON Flattener

This project provides a Haskell library and CLI tool to flatten nested JSON objects into a flat map where keys are represented using dot-notation for objects and index-notation for arrays.

## Usage

1. Build the project using Cabal:
   ```bash
   cabal build
   ```
2. Run the CLI tool:
   ```bash
   cabal run flatten-json-cli -- input.json
   ```

### Example
Input:
```json
{
  "user": {
    "name": "Alice",
    "tags": ["haskell", "functional"]
  }
}
```
Output:
```text
user.name: String "Alice"
user.tags[0]: String "haskell"
user.tags[1]: String "functional"
```