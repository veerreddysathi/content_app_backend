-record(content, {
    id,
    sender_id,
    receiver_id,
    file_type,
    is_payable,
    is_paid = false,
    binary,
    timestamp
}).
