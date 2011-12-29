package org.erlide.core.model.erlang;

public interface ErlangToolkit {

    public abstract IErlParser createParser();

    public abstract IErlScanner createScanner(String scannerName,
            String initialText, String filePath, boolean useCaches);

}
