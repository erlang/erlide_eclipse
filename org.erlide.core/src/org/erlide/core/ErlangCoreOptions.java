package org.erlide.core;

public enum ErlangCoreOptions {

    //@formatter:off
    COMPILER_TASK_TAGS(ErlangCore.PLUGIN_ID + ".compiler.taskTags"), 
    CORE_ENCODING(ErlangCore.PLUGIN_ID + ".encoding");
    //@formatter:on

    private String value;

    private ErlangCoreOptions(final String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }

}
