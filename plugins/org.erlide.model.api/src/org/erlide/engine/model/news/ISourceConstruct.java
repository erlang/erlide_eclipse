package org.erlide.engine.model.news;

@SuppressWarnings("all")
public interface ISourceConstruct {
    TextRange getFullRange();

    TextRange getIdentifyingRange();
}
