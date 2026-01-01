//! CotDB Error Types

pub const Error = error{
    // File errors
    FileNotFound,
    FileExists,
    IoError,
    CorruptedFile,
    InvalidFormat,
    VersionMismatch,

    // Schema errors
    InvalidSchema,
    TableNotFound,
    ColumnNotFound,
    TypeMismatch,

    // Key errors
    KeyNotFound,
    DuplicateKey,
    InvalidKey,

    // Record errors
    RecordNotFound,
    RecordLocked,
    EndOfFile,

    // ULID errors
    InvalidUlid,
    UlidNotFound,

    // Query errors
    SyntaxError,
    InvalidQuery,
    UnknownFunction,

    // Resource errors
    OutOfMemory,
    TooManyTables,
    TooManyColumns,
    TooManyIndexes,
    RecordTooLarge,
};
