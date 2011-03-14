package org.erlide.core;

public enum ErlangCoreOptions {

    COMPILER_TASK_TAGS(ErlangPlugin.PLUGIN_ID + ".compiler.taskTags"),
    CORE_ENCODING(ErlangPlugin.PLUGIN_ID + ".encoding");

    private String value;

    private ErlangCoreOptions(final String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }

}
