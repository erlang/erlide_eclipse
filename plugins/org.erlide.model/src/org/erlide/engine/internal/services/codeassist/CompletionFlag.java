package org.erlide.engine.internal.services.codeassist;

enum CompletionFlag {
    //@formatter:off
    DECLARED_FUNCTIONS,
    EXTERNAL_FUNCTIONS,
    VARIABLES,
    RECORD_FIELDS,
    RECORD_DEFS,
    MODULES,
    MACRO_DEFS,
    IMPORTED_FUNCTIONS,
    AUTO_IMPORTED_FUNCTIONS,
    ARITY_ONLY,
    UNEXPORTED_ONLY,
    INCLUDES,
    INCLUDE_LIBS,
    TYPES
    //@formatter:on
}
