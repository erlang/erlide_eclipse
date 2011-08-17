package org.erlide.core;

public enum ErlangCoreOptions {

    COMPILER_TASK_TAGS(ErlangCore.PLUGIN_ID + ".compiler.taskTags"),
    CORE_ENCODING(ErlangCore.PLUGIN_ID + ".encoding");

    private String value;

    private ErlangCoreOptions(final String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }

}
