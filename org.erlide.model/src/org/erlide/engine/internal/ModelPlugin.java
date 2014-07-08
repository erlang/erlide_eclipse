package org.erlide.engine.internal;

public class ModelPlugin {

    public static final String PLUGIN_ID = "org.erlide.model";
    // FIXME this is kind of an indirect dep on core plugin (needs to be
    // started)
    private static final String CORE_PLUGIN_ID = "org.erlide.core";
    public static final String NATURE_ID = CORE_PLUGIN_ID + ".erlnature";
    public static final String BUILDER_ID = CORE_PLUGIN_ID + ".erlbuilder";

}
