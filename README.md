# DevHub
```bash
npm run validate
git add .
git commit -m "description"
git push
```
## Summary (keep this in mind)
- Keyword = separate entity
- 1 file = 1 concept
- Language = attribute, not owner
- Menu is made of data
- Build just aggregates
- UI is stupid, data are smart

concept = WHAT it is
feature = IF and HOW the language can do it
keyword = HOW it is written

## Feature = answer to the question:
“Does language X support feature Y – and how?”
### For example:
- Multiple inheritance
- Destructors
- Interfaces
- Garbage collection
- Generics
- Async/await

## ID Conventions
### Recommended rules
- lowercase
- dots as hierarchy
- no spaces
- no diacritics
- ID never changes
You change the text, not the ID.
```json
concept:  oop.class
feature:  oop.class.destructor
keyword:  keyword.class
operator: operator.plus
```

## GitHub Actions workflow CI monitor
- ✔ JSON validation against schema
- ✔ ID duplication check
- ✔ ID convention check (regex)
- ✔ check that bindings point to existing IDs
- ✔ build must not crash
