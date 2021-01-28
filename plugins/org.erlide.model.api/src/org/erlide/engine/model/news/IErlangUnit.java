package org.erlide.engine.model.news;

@SuppressWarnings("all")
public interface IErlangUnit extends IErlangElement {
    String getFileExtension();

    Iterable<IErlangForm> getForms();
}
