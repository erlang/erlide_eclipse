package org.erlide.engine.model.news;

@SuppressWarnings("all")
public interface IErlangProject extends IErlangApplication {
    String NATURE_ID = "org.erlide.core.erlnature";

    String getOtpVersion();

    IErlangLibrary getOtpLibrary();
}
