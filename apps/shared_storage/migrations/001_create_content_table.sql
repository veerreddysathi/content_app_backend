CREATE TABLE IF NOT EXISTS content (
    id BIGINT PRIMARY KEY,
    sender_id INTEGER NOT NULL,
    receiver_id INTEGER NOT NULL,
    file_type TEXT NOT NULL,
    is_payable BOOLEAN NOT NULL,
    is_paid BOOLEAN DEFAULT FALSE,
    binary BYTEA NOT NULL,
    timestamp BIGINT NOT NULL
);
