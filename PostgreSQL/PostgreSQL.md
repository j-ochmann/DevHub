# Prerequisites
Docker and docker-compose (or docker compose CLI plugin) installed.
## Step by step process
### Step 1: Create a project directory
Create folder where you will store the configuration file: compose.yaml
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
      PGADMIN_CONFIG_SESSION_COOKIE_SECURE: "True"
      PGADMIN_CONFIG_WTF_CSRF_SSL_STRICT: "False"
    ports:
      - "5050:80" # http://localhost:5050
    depends_on:
      - db
    volumes:
      - pgadmin_data:/var/lib/pgadmin
volumes:
  pg_data:
  pgadmin_data:
```
### Create a local .env file (with password)
Create a file named .env in the same directory that will contain the password.
```yaml
DB_USER=my_user
DB_PASSWORD=my_password
DB_NAME=my_name
DEFAULT_EMAIL=admin@example.com
```
### Important:
- DB_PASSWORD: Change my_password to a strong password.
- DB_NAME: Change my_name to the name of the database you want to create.
- volumes: pg_data:/var/lib/postgresql/data is the key line that creates a permanent (persistent) data store on your host system.
Save the file and exit the editor.
### Step 2: Start the database
In the directory where you have compose.yaml, start the container:
```bash
docker compose up -d
```
- The up command creates and starts the services defined in the file.
- The -d (detached) flag means that the container will run in the background and you can continue to use the terminal.
Docker will download the PostgreSQL image (if you don't already have it) and start the database server.
### Step 3: Verify that the database is running
Check the status of the running containers:
```bash
docker ps
```
