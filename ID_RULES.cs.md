# ID konvence projektu

Tento dokument definuje **jednotné, neměnné ID konvence** pro celý projekt. ID jsou **primární klíče** všech dat a slouží k:

* propojování entit (concept ↔ feature ↔ keyword)
* generování menu a navigace
* fulltext indexům
* exportům (CSV / SQL / SQLite)
* dlouhodobé stabilitě projektu

> **Zlaté pravidlo:** Text se může měnit. **ID nikdy.**

---

## 1. Obecná pravidla (platí pro všechna ID)

### Povolené znaky

* malá písmena `a–z`
* číslice `0–9`
* tečka `.` jako hierarchický oddělovač

### Zakázané

* velká písmena
* mezery
* diakritika
* pomlčky `-`
* podtržítka `_` (povoleno pouze ve výjimečných případech u keywordů, pokud to odpovídá syntaxi jazyka)

### Formát

```text
<namespace>.<domain>.<entity>[.<subentity>...]
```

### Příklady

```text
oop.class
oop.class.destructor
keyword.async_await
```

---

## 2. Namespace přehled

| Entita   | Namespace     |
| -------- | ------------- |
| Concept  | `concept.`    |
| Feature  | `feature.`    |
| Keyword  | `keyword.`    |
| Literal  | `literal.`    |
| Operator | `operator.`   |
| Paradigm | `paradigm.`   |

---

## 3. Concept ID

### Účel

Abstraktní pojmy programování nezávislé na syntaxi.

### Formát

```text
<domain>.<concept>[.<subconcept>]
```

### Příklady

```text
oop.class
oop.inheritance
types.generics
control.loop
```

### Pravidla

* **nesmí obsahovat název jazyka**
* musí být srozumitelné i bez kontextu
* hierarchie jde od obecného ke konkrétnímu

---

## 4. Feature ID

### Účel

Porovnatelné vlastnosti jazyků (jestli a jak něco podporují).

### Formát

```text
<domain>.<concept>.<feature>
```

### Příklady

```text
oop.class.constructor
oop.class.destructor
oop.inheritance.multiple
memory.garbage_collection
```

### Pravidla

* vždy navazuje na existující concept
* **1 feature = 1 otázka typu ano/ne (+ poznámky)**
* musí být porovnatelná napříč jazyky

---

## 5. Keyword ID

### Účel

Lexikální prvky programovacích jazyků (slovník).

### Formát

```text
keyword.<token>
```

### Příklady

```text
keyword.class
keyword.interface
keyword.try
keyword.async_await
```

### Pravidla

* token odpovídá skutečné syntaxi jazyka
* pokud se keyword liší napříč jazyky (`extends` vs `:`), mají **samostatná ID**

---

## 6. Operator ID

### Formát

```text
operator.<name>
```

### Příklady

```text
operator.plus
operator.arrow
operator.scope_resolution
```

---

## 7. Literal ID

### Formát

```text
literal.<value>
```

### Příklady

```text
literal.null
literal.true
literal.false
```

---

## 8. Vazby mezi ID

Vazby se realizují **výhradně přes ID** (string).

### Povolené vazby

```text
concept → relatedFeatures
feature → relatedConcepts
feature → relatedKeywords
keyword → relatedConcepts
keyword → relatedFeatures
```

### Zakázané

* odkazy na soubory
* relativní cesty
* vnořená data místo ID

---

## 9. Stabilita a verzování

* ID se **nikdy nemění**
* pokud se pojem přejmenuje → změní se `title` / `description`
* pokud je pojem zrušen → zůstává ID, ale označí se jako deprecated (budoucí rozšíření)

---

## 10. Kontrolní checklist (před commitem)

* [ ] ID odpovídá konvenci
* [ ] ID je jedinečné
* [ ] Vazby směřují na existující ID
* [ ] Texty jsou oddělené od ID

---

**Tento dokument je závazný pro celý projekt.**
Jakýkoliv nový typ entity MUSÍ mít vlastní namespace a být sem doplněn.
