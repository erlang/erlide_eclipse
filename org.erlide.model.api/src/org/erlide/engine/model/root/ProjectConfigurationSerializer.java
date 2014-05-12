package org.erlide.engine.model.root;

public interface ProjectConfigurationSerializer {

    /**
     * Encode the project properties in a string (as it would look like in the
     * config file).
     */
    String encodeConfig(ErlangProjectProperties info);

    /**
     * Decode project properties from string (as read from config file).
     *
     * TODO: store encoding
     *
     * TODO: store config text as template to use for encoding
     */
    ErlangProjectProperties decodeConfig(String config);

}
