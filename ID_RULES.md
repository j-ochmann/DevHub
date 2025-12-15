# Project ID Conventions

This document defines **uniform, immutable ID conventions** for the entire project. IDs are the **primary keys** of all data and are used for:

* linking entities (concept ↔ feature ↔ keyword)
* generating menus and navigation
* full-text indexing
* exports (CSV / SQL / SQLite)
* long-term project stability

> **Golden rule:** Text may change. **IDs never do.**

---

## 1. General Rules (apply to all IDs)

### Allowed characters

* lowercase letters `a–z`
* digits `0–9`
* dot `.` as a hierarchical separator

### Forbidden

* uppercase letters
* spaces
* diacritics
* hyphens `-`
* underscores `_` (allowed only in exceptional cases for keywords if they reflect real language syntax)

### Format

```text
<namespace>.<domain>.<entity>[.<subentity>...]
```

### Examples

```text
oop.class
oop.class.destructor
keyword.async_await
```

---

## 2. Namespace Overview

| Entity   | Namespace   |
| -------- | ----------- |
| Concept  | (no prefix) |
| Feature  | (no prefix) |
| Keyword  | `keyword.`  |
| Operator | `operator.` |
| Literal  | `literal.`  |

---

## 3. Concept IDs

### Purpose

Abstract programming concepts independent of syntax.

### Format

```text
<domain>.<concept>[.<subconcept>]
```

### Examples

```text
oop.class
oop.inheritance
types.generics
control.loop
```

### Rules

* **must not contain a language name**
* must be understandable without context
* hierarchy goes from general to specific

---

## 4. Feature IDs

### Purpose

Comparable language capabilities (whether and how a language supports something).

### Format

```text
<domain>.<concept>.<feature>
```

### Examples

```text
oop.class.constructor
oop.class.destructor
oop.inheritance.multiple
memory.garbage_collection
```

### Rules

* always builds on an existing concept
* **1 feature = 1 yes/no question (+ notes)**
* must be comparable across languages

---

## 5. Keyword IDs

### Purpose

Lexical elements of programming languages (dictionary layer).

### Format

```text
keyword.<token>
```

### Examples

```text
keyword.class
keyword.interface
keyword.try
keyword.async_await
```

### Rules

* token must match real language syntax
* if keywords differ across languages (`extends` vs `:`), they have **separate IDs**

---

## 6. Operator IDs

### Format

```text
operator.<name>
```

### Examples

```text
operator.plus
operator.arrow
operator.scope_resolution
```

---

## 7. Literal IDs

### Format

```text
literal.<value>
```

### Examples

```text
literal.null
literal.true
literal.false
```

---

## 8. Relationships Between IDs

Relationships are expressed **exclusively via IDs** (strings).

### Allowed relationships

```text
concept → relatedFeatures
feature → relatedConcepts
feature → relatedKeywords
keyword → relatedConcepts
keyword → relatedFeatures
```

### Forbidden

* file references
* relative paths
* embedded objects instead of IDs

---

## 9. Stability and Versioning

* IDs **must never change**
* if a term is renamed → update `title` / `description`
* if a concept is removed → keep the ID and mark it as deprecated (future extension)

---

## 10. Pre-commit Checklist

* [ ] ID follows the convention
* [ ] ID is unique
* [ ] References point to existing IDs
* [ ] Text is separated from IDs

---

**This document is binding for the entire project.**
Any new entity type MUST define its own namespace and be added here.
