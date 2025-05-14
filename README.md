# Company K Backend (content_app_backend) - Proof of Concept

This project is a microservices-based Erlang backend infrastructure designed to support digital content distribution between content producers and consumers. It includes RESTful APIs for upload and retrieval, a queue for asynchronous delivery, and PostgreSQL for durable data storage.

---

## Folder Structure

```
company_k/
├── apps/
│   ├── sender_service/         # API for content producers to upload files
│   ├── consumer_service/       # API for users to retrieve files and trigger payments
│   ├── shared_storage/         # PostgreSQL database access and migrations
│   └── queue_consumer/         # Background service to consume messages from RabbitMQ
├── docker-compose.yml          # Orchestrates PostgreSQL, RabbitMQ, and apps
├── rebar.config                # Top-level Rebar3 config
├── Makefile                    # Dev commands for building, running, migrating
└── README.md                   # Project documentation
```

---

## Sub Applications

### 1. `sender_service`

* A REST API that accepts:

  * Binary content
  * Metadata: sender\_id, receiver\_id, file\_type, is\_payable
* Publishes messages to RabbitMQ (`content_uploads` queue).

### 2. `consumer_service`

* A REST API that supports:

  * Querying content by sender\_id
  * Retrieving binary files
  * Marking content as paid (simulates payment)

### 3. `shared_storage`

* Provides the data layer for the system.
* Connects to PostgreSQL using `postgrex`.
* Defines and runs DB migrations.
* Exposes an `insert_content/7` function to save data.

### 4. `queue_consumer`

* Background service that:

  * Listens to `content_uploads` RabbitMQ queue
  * Processes messages with content metadata and binary
  * Stores the data in the database via `shared_storage`

---

## Setup & Running the Project

### 1. Prerequisites

* Docker and Docker Compose
* Erlang/OTP 25+
* Rebar3

### 2. Build and Start All Services

```bash
make all
make run
```

This will:

* Compile the project with Rebar3
* Build Docker images for each app
* Run `docker-compose` to start services (PostgreSQL, RabbitMQ, sender, consumer, queue\_consumer)

### 3. Run Migrations

To create the `content` table:

```bash
make migrate
```

### 4. Stop the Project

```bash
make stop
```

---

## Individual Commands

### Compile Only

```bash
rebar3 compile
```

### Run Only a Single Service (e.g., sender\_service)

```bash
docker-compose up sender_service
```

### Clean Build Artifacts and Volumes

```bash
make clean
```

---

## API Endpoints

### Sender Service (port 8081)

#### `POST /upload`

* Upload a content file and metadata.
* **Body (multipart/form-data):**

  * `sender_id`: integer
  * `receiver_id`: integer
  * `file_type`: string
  * `is_payable`: boolean
  * `file`: binary file

**Example:**

```bash
curl -X POST http://localhost:8081/upload \
  -F sender_id=1 \
  -F receiver_id=42 \
  -F file_type="pdf" \
  -F is_payable=true \
  -F file=@"example.pdf"
```

---

### Consumer Service (port 8082)

#### `GET /content?sender_id=1`

* Fetch a list of available content for a specific sender.

#### `GET /content/:id`

* Download content by its ID.

#### `POST /content/:id/pay`

* Trigger a mock payment for payable content.

**Example:**

```bash
curl -X POST http://localhost:8082/content/123/pay
```

---

## Database Table

```sql
CREATE TABLE content (
    id BIGSERIAL PRIMARY KEY,
    sender_id INTEGER NOT NULL,
    receiver_id INTEGER NOT NULL,
    file_type TEXT NOT NULL,
    is_payable BOOLEAN NOT NULL,
    is_paid BOOLEAN DEFAULT FALSE,
    binary BYTEA NOT NULL,
    timestamp BIGINT NOT NULL
);
```

---
