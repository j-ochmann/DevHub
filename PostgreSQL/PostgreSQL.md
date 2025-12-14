# Předpoklady
Nainstalovaný docker a docker-compose (nebo docker compose CLI plugin).
## Postup krok za krokem
### Krok 1: Vytvořte adresář projektu
Vytvořte novou složku, kam uložíte konfigurační soubor: compose.yaml
```yaml
services:
  db:
    image: postgres:16
    restart: always
    #container_name: ${CONTAINER_NAME}
    environment:
      POSTGRES_USER: ${DB_USER}
      POSTGRES_PASSWORD: ${DB_PASSWORD}
      POSTGRES_DB: ${DB_NAME}
    ports:
      - "5432:5432"
    volumes:
      - pg_data:/var/lib/postgresql/data
  pgadmin:
    image: dpage/pgadmin4:latest
    restart: always
    environment:
      PGADMIN_DEFAULT_EMAIL: ${DEFAULT_EMAIL}
      PGADMIN_DEFAULT_PASSWORD: ${DB_PASSWORD}
      PGADMIN_CONFIG_ENHANCED_COOKIE_PROTECTION: "False"
      PGADMIN_CONFIG_SESSION_COOKIE_SECURE: "True"
      PGADMIN_CONFIG_SESSION_COOKIE_SAMESITE: "None"
      PGADMIN_CONFIG_WTF_CSRF_SSL_STRICT: "False"
      PGADMIN_CONFIG_PREFERRED_URL_SCHEME: "https"
    ports:
      - "5050:80" # http://localhost:8080
    depends_on:
      - db
    volumes:
      - pgadmin_data:/var/lib/pgadmin
volumes:
  pg_data:
  pgadmin_data:
```
### Vytvořte lokální soubor .env (s heslem)
Vytvořte ve stejném adresáři soubor s názvem .env, který bude obsahovat heslo.
```yaml
DB_USER=my_user
DB_PASSWORD=my_password
DB_NAME=my_name
DEFAULT_EMAIL=admin@example.com
```
### Důležité:
 - DB_PASSWORD: Změňte my_password na silné heslo.
 - DB_NAME: Změňte my_name na název databáze, kterou chcete vytvořit.
 - volumes: pg_data:/var/lib/postgresql/data je klíčový řádek, který vytváří trvalé (perzistentní) úložiště dat na vašem hostitelském systému.
Uložte soubor a ukončete editor.
### Krok 2: Spusťte databázi
V adresáři, kde máte compose.yaml, spusťte kontejner:
```bash
docker compose up -d
```
 - Příkaz up vytvoří a spustí služby definované v souboru.
 - Příznak -d (detached) znamená, že se kontejner spustí na pozadí a vy můžete dál používat terminál.
Docker stáhne obraz PostgreSQL (pokud ho ještě nemáte) a spustí databázový server.
### Krok 3: Ověřte, že databáze běží
Zkontrolujte stav spuštěných kontejnerů:
```bash
docker ps
```

