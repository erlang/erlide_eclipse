package org.erlide.engine.model.news;

@SuppressWarnings("all")
public interface IErlangEbinFolder extends IErlangFolder {
    Iterable<? extends IErlangBeam> getBeams();

    IErlangBeam getBeam(final String name);
}
